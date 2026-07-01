#install.packages(c("sf","arcgisgeocode","yyjsonr","dplyr","gdalraster"))
library(sf)
library(arcgisgeocode)
library(yyjsonr)
library(dplyr)
library(gdalraster)

# ---------------------------------------------------------------------------
# District layer naming helpers
# ---------------------------------------------------------------------------

# Turn an arbitrary label into a safe column-name prefix: non-alphanumeric runs
# collapse to a single underscore, and leading/trailing underscores are trimmed.
# e.g. "State Senate (2020)" -> "State_Senate_2020"
sanitize_layer_name <- function(name) {
  name <- gsub("[^A-Za-z0-9]+", "_", name)
  gsub("^_+|_+$", "", name)
}

# Prefix a district layer's attribute (non-geometry) columns with its layer name
# so columns from multiple layers stay distinct and self-describing. A "DISTRICT"
# column in the "Congressional" layer becomes "Congressional_DISTRICT". The
# geometry column is left untouched. An empty/blank name is a no-op.
prefix_layer_columns <- function(layer, layer_name) {
  layer_name <- sanitize_layer_name(layer_name)
  if (nchar(layer_name) == 0) return(layer)

  geom_col <- attr(layer, "sf_column")
  data_cols <- setdiff(names(layer), geom_col)
  if (length(data_cols) > 0) {
    idx <- match(data_cols, names(layer))
    names(layer)[idx] <- paste0(layer_name, "_", data_cols)
  }
  layer
}

# Determine a display name for each district layer, used to prefix its columns.
# Precedence, per layer:
#   1. an explicit districtNames value (if supplied and non-blank)
#   2. the name attached to the districtsList element (named list)
#   3. the file's base name, when the element is still a file path
#   4. a positional fallback ("District_1", "District_2", ...)
resolve_layer_names <- function(districtsList, districtNames = NULL) {
  list_names <- names(districtsList)
  vapply(seq_along(districtsList), function(i) {
    if (!is.null(districtNames) && length(districtNames) >= i && nzchar(districtNames[i])) {
      return(districtNames[i])
    }
    if (!is.null(list_names) && nzchar(list_names[i])) {
      return(list_names[i])
    }
    el <- districtsList[[i]]
    if (is.character(el) && length(el) == 1) {
      return(tools::file_path_sans_ext(basename(el)))
    }
    paste0("District_", i)
  }, character(1))
}

# AssignDistricts()
# Geocodes a member list and assigns each address to the district(s) it falls
# within, across one or more district layers.
#
# Arguments:
#   memberList    - a data.frame, or a path to a CSV, of addresses to assign.
#   StreetCol     - name of the street-address column in memberList.
#   CityCol       - name of the city column in memberList.
#   districtsList - a single district layer/path, or a (optionally named) list
#                   of district layers/paths (.shp or .geojson). Names, when
#                   present, are used to prefix that layer's output columns.
#   removeGEO     - if TRUE (default) the geometry column is dropped from the
#                   returned table for clean CSV export.
#   districtNames - optional character vector, one name per district layer, used
#                   to prefix each layer's columns and disambiguate them in the
#                   output. Overrides names(districtsList). See resolve_layer_names().
AssignDistricts <- function(memberList, StreetCol="Street.Address", CityCol="City", districtsList, removeGEO = TRUE, districtNames = NULL){
  # Member List Path - read if it's a file path, otherwise use as-is
  if (is.character(memberList) && length(memberList) == 1) {
    memberList <- read.csv(memberList)
  }

  # Use the selected columns for Street.Address and City
  memberList$Street.Address <- memberList[[StreetCol]]
  memberList$City <- memberList[[CityCol]]

  # Store original row count and give every row a stable identifier. Nothing is
  # ever dropped from here on: rows that can't be geocoded or assigned are FLAGGED
  # via the `geocode_status` column and carried through to the output, so a few
  # bad rows never derail the whole run.
  original_count <- nrow(memberList)
  memberList$original_row_id <- seq_len(nrow(memberList))

  # Flag rows whose street address or city is blank/NA. These can't be geocoded,
  # but they stay in the table with a status of "Missing address".
  valid_addresses <- !is.na(memberList$Street.Address) &
                     memberList$Street.Address != "" &
                     !is.na(memberList$City) &
                     memberList$City != ""

  memberList$geocode_status <- ifelse(valid_addresses, "OK", "Missing address")

  cat(paste("Number of geocodable addresses:", sum(valid_addresses), "\n"))

  read_spatial_file <- function(file_path) {
    # Get the file extension
    file_ext <- tolower(tools::file_ext(file_path))

    if (length(file_ext) != 1) {
      stop("Invalid file extension")
    }

    # Read the file based on its extension
    spatial_object <- switch(
      file_ext,
      "shp" = {
        sf::sf_use_s2(FALSE)
        # Set GDAL configuration option
        old_config <- gdalraster::get_config_option("SHAPE_RESTORE_SHX")
        gdalraster::set_config_option("SHAPE_RESTORE_SHX", "YES")
        on.exit(gdalraster::set_config_option("SHAPE_RESTORE_SHX", old_config))

        districts <- sf::st_read(file_path)
        districts <- sf::st_make_valid(districts)
        districts <- sf::st_buffer(districts, 0)
        districts
      },
      "geojson" = {
        districts <- yyjsonr::read_geojson_file(file_path)
        districts <- sf::st_make_valid(districts)
        districts <- sf::st_buffer(districts, 0)
        districts
      },
      {
        stop("Unsupported file format. Please provide a .shp or .geojson file.")
      }
    )

    return(spatial_object)
  }

  # Convert districtsList to a list if needed. A single sf object is itself a
  # list, so wrap anything that isn't already a list-of-layers.
  if (!inherits(districtsList, "list")) {
    districtsList <- list(districtsList)
  }

  # Resolve a name for each layer BEFORE reading, so file-path basenames are
  # still available to fall back on (reading replaces paths with sf objects).
  layer_names <- resolve_layer_names(districtsList, districtNames)

  # Read spatial files if they're file paths
  for (i in seq_along(districtsList)) {
    if (is.character(districtsList[[i]]) && length(districtsList[[i]]) == 1) {
      districtsList[[i]] <- read_spatial_file(districtsList[[i]])
    }
  }

  # Ensure all districts have valid geometry
  districtsList <- lapply(districtsList, sf::st_make_valid)

  # Prefix each layer's attribute columns with its resolved name so that columns
  # from different layers stay distinct in the joined output (see #4).
  districtsList <- Map(prefix_layer_columns, districtsList, layer_names)

  # BBOX from first district
  boundaries <- sf::st_bbox(districtsList[[1]])

  # --- Geocoding -----------------------------------------------------------
  # Geocode only the rows that have a usable address. arcgisgeocode returns a
  # `result_id` column that is a 1-based index back into the vector of addresses
  # we sent. We use it to place each result onto the exact source row instead of
  # binding by position. This matters because the service can return FEWER rows
  # than we sent (an address may yield no candidate) -- a positional cbind would
  # then silently misalign every following row. Keying on result_id keeps rows
  # aligned and lets unmatched inputs simply stay NA, so we no longer need the
  # old hard stop() on a row-count mismatch.
  valid_idx <- which(valid_addresses)

  if (length(valid_idx) > 0) {
    cat("Geocoding addresses...\n")
    memberGeos <- arcgisgeocode::find_address_candidates(
      address = memberList$Street.Address[valid_idx],
      city = memberList$City[valid_idx],
      search_extent = boundaries,
      max_locations = 1
    )
    cat(paste("Number of geocoded results:", nrow(memberGeos), "\n"))

    # Carry every geocode field back onto the full member list, keyed by
    # result_id. Columns are pre-filled with NA so any row without a match
    # (or without a usable address) stays NA rather than misaligning.
    geo_df <- if (inherits(memberGeos, "sf")) sf::st_drop_geometry(memberGeos) else memberGeos

    # result_id maps each returned candidate back to its input row (1-based). If a
    # future package version omits it, fall back to positional alignment only when
    # the counts match exactly -- otherwise stop rather than misalign silently.
    if ("result_id" %in% names(geo_df)) {
      result_ids <- geo_df$result_id
    } else if (nrow(geo_df) == length(valid_idx)) {
      result_ids <- seq_len(nrow(geo_df))
    } else {
      stop("Geocoder returned no 'result_id' and an unexpected row count; cannot align results safely.")
    }

    geo_cols <- setdiff(names(geo_df), "result_id")
    target_rows <- valid_idx[result_ids]
    for (col in geo_cols) {
      if (is.null(memberList[[col]])) memberList[[col]] <- NA
      memberList[[col]][target_rows] <- geo_df[[col]]
    }
  } else {
    cat("No geocodable addresses found.\n")
  }

  # Guarantee x/y coordinate columns exist even if geocoding produced none.
  if (is.null(memberList$x)) memberList$x <- NA_real_
  if (is.null(memberList$y)) memberList$y <- NA_real_

  # Flag address-valid rows that still failed to geocode (no candidate returned).
  failed_geo <- valid_addresses & (is.na(memberList$x) | is.na(memberList$y))
  memberList$geocode_status[failed_geo] <- "No geocode match"

  cat(paste("Number of geocoded addresses:", sum(!is.na(memberList$x) & !is.na(memberList$y)), "\n"))

  # --- Spatial assignment --------------------------------------------------
  # Turn EVERY row into a point. Rows with NA coordinates (missing address or no
  # geocode match) become empty points via na.fail = FALSE; a left st_join then
  # keeps them in the output with NA district columns. Nothing is dropped.
  member_pts <- memberList |>
    sf::st_as_sf(
      coords = c("x","y"),
      crs = sf::st_crs("EPSG:4326"),
      na.fail = FALSE
    ) |>
    sf::st_transform(crs = sf::st_crs(districtsList[[1]]))

  # Join with the first district layer
  final <- sf::st_join(member_pts, districtsList[[1]], join = sf::st_within)

  # Join with additional district layers if they exist
  if (length(districtsList) > 1) {
    for (i in 2:length(districtsList)) {
      # Transform to match CRS
      districtsList[[i]] <- districtsList[[i]] |>
        sf::st_transform(crs = sf::st_crs(districtsList[[1]]))

      # Spatial join
      final <- sf::st_join(final, districtsList[[i]], join = sf::st_within)
    }
  }

  # Drop geometry for a clean tabular/CSV result unless the caller wants it.
  result <- if (isTRUE(removeGEO)) {
    sf::st_drop_geometry(final)
  } else {
    final
  }

  # --- Summary -------------------------------------------------------------
  # Counts are taken from the flag column on the (row-complete) member list so
  # they reflect input rows, independent of any join row-multiplication.
  invalid_count <- sum(memberList$geocode_status == "Missing address")
  failed_count  <- sum(memberList$geocode_status == "No geocode match")
  success_count <- sum(memberList$geocode_status == "OK")

  status_cols <- c("original_row_id", "Street.Address", "City", "geocode_status")
  invalid_rows <- memberList[memberList$geocode_status == "Missing address", status_cols]
  failed_rows  <- memberList[memberList$geocode_status == "No geocode match", status_cols]

  cat("\n=== SUMMARY ===\n")
  cat(sprintf("Total addresses: %d\n", original_count))
  cat(sprintf("Invalid addresses (blank/NA): %d\n", invalid_count))
  cat(sprintf("Failed geocoding: %d\n", failed_count))
  cat(sprintf("Successfully geocoded: %d\n", success_count))

  if (nrow(failed_rows) > 0) {
    cat("\nFailed addresses:\n")
    for (i in 1:nrow(failed_rows)) {
      cat(sprintf("  Row %d: %s, %s\n",
                  failed_rows$original_row_id[i],
                  failed_rows$Street.Address[i],
                  failed_rows$City[i]))
    }
  }
  cat("===============\n\n")

  # Attach summary statistics as attributes
  attr(result, "summary_stats") <- list(
    original_count = original_count,
    invalid_address_count = invalid_count,
    invalid_rows = if(invalid_count > 0) invalid_rows else NULL,
    failed_geocoding_count = failed_count,
    failed_geocoding_rows = if(failed_count > 0) failed_rows else NULL,
    successful_count = success_count
  )

  return(result)
}