# =============================================================================
# districting_core.R
#
# Shared core logic for both the command-line script (AssignDistricts.R) and the
# Shiny app (DistrictingApp.R). Both entry points source() this file so the
# geocoding + district-assignment behavior lives in exactly one place.
#
# The pipeline is deliberately split into small, reusable pieces:
#
#   prepare_district_layers()  read + validate + name-prefix one or more layers
#   compute_search_extent()    union bounding box of all layers (for geocoding)
#   GeocodeMembers()           geocode a member list ONCE -> points + status
#   AssignToDistricts()        join geocoded points to prepared district layers
#   AssignDistricts()          one-shot convenience wrapper around the above
#
# Splitting GeocodeMembers() from AssignToDistricts() is what enables the
# "geocode once, apply to many districting files" workflow: geocode a member
# list a single time, then reuse those points across any number of layer sets.
#
# This file assumes the following packages are available (the entry-point files
# load them): sf, arcgisgeocode, yyjsonr, dplyr, gdalraster. Functions reference
# packages with :: so no library() calls are needed here.
# =============================================================================


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


# ---------------------------------------------------------------------------
# Reading district geometry
# ---------------------------------------------------------------------------

# Read a single .shp or .geojson file from a path and return a validated,
# repaired sf layer.
read_spatial_file <- function(file_path) {
  file_ext <- tolower(tools::file_ext(file_path))

  if (length(file_ext) != 1) {
    stop("Invalid file extension")
  }

  spatial_object <- switch(
    file_ext,
    "shp" = {
      sf::sf_use_s2(FALSE)
      # Let GDAL rebuild a missing .shx sidecar, restoring the option afterwards.
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

# Unzip a single uploaded district archive and read the spatial layer it contains.
#
# IMPORTANT: each call extracts into its OWN unique directory (via tempfile()).
# Using the shared session tempdir() would let multiple uploaded archives land in
# the same folder, so a recursive list.files(...)[1] could match a file from a
# DIFFERENT archive -- collapsing every upload onto one layer. Isolating each
# archive guarantees the glob only sees the file(s) from the archive being read.
#
# Returns: an sf object with validated, repaired geometry.
unzip_and_read_spatial <- function(zipfile) {
  temp_dir <- tempfile("district_")
  dir.create(temp_dir)
  utils::unzip(zipfile, exdir = temp_dir)

  shp_files <- list.files(temp_dir, pattern = "\\.shp$", full.names = TRUE, recursive = TRUE)
  geojson_files <- list.files(temp_dir, pattern = "\\.geojson$", full.names = TRUE, recursive = TRUE)

  if (length(shp_files) > 0) {
    sf::sf_use_s2(FALSE)
    old_config <- gdalraster::get_config_option("SHAPE_RESTORE_SHX")
    gdalraster::set_config_option("SHAPE_RESTORE_SHX", "YES")
    on.exit(gdalraster::set_config_option("SHAPE_RESTORE_SHX", old_config))

    districts <- sf::read_sf(shp_files[1])
    districts <- sf::st_make_valid(districts)
    districts <- sf::st_buffer(districts, 0)
    return(districts)
  } else if (length(geojson_files) > 0) {
    districts <- yyjsonr::read_geojson_file(geojson_files[1])
    districts <- sf::st_make_valid(districts)
    districts <- sf::st_buffer(districts, 0)
    return(districts)
  } else {
    stop("No .shp or .geojson file found in the zip archive")
  }
}


# ---------------------------------------------------------------------------
# Preparing district layers
# ---------------------------------------------------------------------------

# Normalize, read, validate and name-prefix one or more district layers.
#
# districtsList : a single layer/path, or an (optionally named) list of layers
#                 and/or file paths (.shp / .geojson). sf objects pass through;
#                 character paths are read via read_spatial_file().
# districtNames : optional character vector of names, one per layer, used to
#                 prefix each layer's columns (see resolve_layer_names()).
#
# Returns: a list of prepared sf layers, each with validated geometry and
# name-prefixed attribute columns. Ready to hand to AssignToDistricts().
prepare_district_layers <- function(districtsList, districtNames = NULL) {
  # A single sf object is itself a list, so wrap anything that isn't already a
  # list-of-layers. inherits(x, "list") is TRUE only for real lists (not
  # data.frames/sf), so a lone layer is wrapped correctly.
  if (!inherits(districtsList, "list")) {
    districtsList <- list(districtsList)
  }

  # Resolve names BEFORE reading, so file-path basenames are still available to
  # fall back on (reading replaces paths with sf objects).
  layer_names <- resolve_layer_names(districtsList, districtNames)

  # Read any file-path elements into sf layers.
  for (i in seq_along(districtsList)) {
    if (is.character(districtsList[[i]]) && length(districtsList[[i]]) == 1) {
      districtsList[[i]] <- read_spatial_file(districtsList[[i]])
    }
  }

  # Ensure valid geometry, then prefix each layer's columns with its name so
  # columns from different layers stay distinct in the joined output.
  districtsList <- lapply(districtsList, sf::st_make_valid)
  districtsList <- Map(prefix_layer_columns, districtsList, layer_names)

  districtsList
}

# Union bounding box of all layers, reprojected to WGS84 (EPSG:4326), suitable
# for passing to the geocoder as a search_extent. Using the union (rather than
# just one layer) means the box covers the TOTAL area your districts span, so
# nothing you care about falls outside it.
#
# Geocoding with a search_extent is opt-in (see AssignDistricts / the app's
# "Restrict geocoding to district area" checkbox). It is off by default because
# search_extent hard-limits candidates to inside the box: a valid address just
# outside every district would return no candidate and be mis-flagged as a
# geocode failure, when really it's "found, just unassigned".
#
# `pad` expands the box by a fraction of its width/height on every side (default
# 5%). This is an invisible robustness detail -- it keeps edge addresses from
# falsely failing without the user needing to reason about it.
compute_search_extent <- function(layers, pad = 0.05) {
  boxes <- lapply(layers, function(layer) sf::st_bbox(sf::st_transform(layer, 4326)))
  xmin <- min(vapply(boxes, function(b) b[["xmin"]], numeric(1)))
  ymin <- min(vapply(boxes, function(b) b[["ymin"]], numeric(1)))
  xmax <- max(vapply(boxes, function(b) b[["xmax"]], numeric(1)))
  ymax <- max(vapply(boxes, function(b) b[["ymax"]], numeric(1)))

  # Pad by a fraction of each span so edge addresses still geocode.
  dx <- (xmax - xmin) * pad
  dy <- (ymax - ymin) * pad

  sf::st_bbox(
    c(xmin = xmin - dx, ymin = ymin - dy, xmax = xmax + dx, ymax = ymax + dy),
    crs = sf::st_crs(4326)
  )
}


# ---------------------------------------------------------------------------
# Geocoding
# ---------------------------------------------------------------------------

# Geocode a member list ONCE and return every input row, flagged and (where
# possible) coordinated. No rows are dropped.
#
# memberList : a data.frame, or a path to a CSV, of addresses.
# StreetCol  : name of the street-address column.
# CityCol    : name of the city column.
# boundaries : optional bbox (see compute_search_extent) to steer geocoding.
# verbose    : print progress to the console.
#
# Returns: the member data.frame with these columns added/populated:
#   original_row_id  stable 1..N identifier for mapping results back
#   Street.Address, City  the resolved address/city used for geocoding
#   geocode_status   "OK" | "Missing address" | "No geocode match"
#   geo_x, geo_y     longitude/latitude (NA where not geocoded)
#   geo_*            any additional geocoder fields (geo_score, geo_match_addr...)
GeocodeMembers <- function(memberList, StreetCol, CityCol, boundaries = NULL, verbose = TRUE) {
  if (is.character(memberList) && length(memberList) == 1) {
    memberList <- read.csv(memberList)
  }

  memberList$Street.Address <- memberList[[StreetCol]]
  memberList$City <- memberList[[CityCol]]

  if (is.null(memberList$original_row_id)) {
    memberList$original_row_id <- seq_len(nrow(memberList))
  }

  # Row count and identifiers captured up front. memberList is only ever assigned
  # INTO from here (never subset or reordered), so these must still hold at the
  # end -- see the invariant check below. This is the backbone of "absolutely
  # match rows".
  n_input <- nrow(memberList)
  original_row_id_before <- memberList$original_row_id

  # Flag rows whose street or city is blank -- NA, empty, or whitespace-only.
  # They can't be geocoded but stay in the table with status "Missing address".
  street_blank <- is.na(memberList$Street.Address) | trimws(memberList$Street.Address) == ""
  city_blank   <- is.na(memberList$City)           | trimws(memberList$City) == ""
  valid_addresses <- !street_blank & !city_blank

  memberList$geocode_status <- ifelse(valid_addresses, "OK", "Missing address")
  if (verbose) cat(paste("Number of geocodable addresses:", sum(valid_addresses), "\n"))

  valid_idx <- which(valid_addresses)

  if (length(valid_idx) > 0) {
    if (verbose) cat("Geocoding addresses...\n")

    args <- list(
      address = memberList$Street.Address[valid_idx],
      city = memberList$City[valid_idx],
      max_locations = 1
    )
    if (!is.null(boundaries)) args$search_extent <- boundaries
    memberGeos <- do.call(arcgisgeocode::find_address_candidates, args)

    if (verbose) cat(paste("Number of geocoded results:", nrow(memberGeos), "\n"))

    # Carry every geocode field back onto the full member list, keyed by the
    # input row each candidate came from. find_address_candidates() tags every
    # returned candidate with `input_id`, a 1-based index into the addresses we
    # sent (in the package source the results are cbind()ed with
    # input_id = rep(seq_along(inputs), n_candidates_per_input)). Using it places
    # each result on the EXACT source row instead of binding by position -- which
    # matters because inputs that return NO candidate are simply absent from the
    # response, so a positional bind would shift every following row.
    #
    # NOTE: do NOT key on `result_id`. That is a field of the *batch*
    # geocode_addresses() endpoint; here it appears among the geocoder's
    # attribute columns as all-NA (it is only populated when you pass input IDs),
    # so keying on it drops every row -> "nothing geocoded".
    #
    # Geocoder fields are prefixed with "geo_" (geo_x, geo_y, geo_score, ...) so
    # they can never overwrite a same-named column in the member CSV (a CSV with
    # its own "x"/"y"/"score" column would otherwise be clobbered).
    geo_df <- if (inherits(memberGeos, "sf")) sf::st_drop_geometry(memberGeos) else memberGeos

    if ("input_id" %in% names(geo_df)) {
      input_ids <- suppressWarnings(as.integer(geo_df[["input_id"]]))
    } else if (nrow(geo_df) == length(valid_idx)) {
      # Fall back to positional alignment only when counts match exactly.
      input_ids <- seq_len(nrow(geo_df))
    } else {
      stop("Geocoder response is missing 'input_id' and the row count is unexpected; cannot align results safely.")
    }

    # Keep the first candidate per input (max_locations = 1 already limits to one,
    # but guard against repeats), and only ids that point to a real input row;
    # anything else is left NA and flagged "No geocode match" below.
    sel <- !duplicated(input_ids) &
           !is.na(input_ids) &
           input_ids >= 1 &
           input_ids <= length(valid_idx)
    target_rows <- valid_idx[input_ids[sel]]

    geo_cols <- setdiff(names(geo_df), c("input_id", "result_id"))
    for (col in geo_cols) {
      out_col <- paste0("geo_", col)
      if (is.null(memberList[[out_col]])) memberList[[out_col]] <- NA
      memberList[[out_col]][target_rows] <- geo_df[[col]][sel]
    }
  } else if (verbose) {
    cat("No geocodable addresses found.\n")
  }

  # Guarantee coordinate columns exist even if geocoding produced none.
  if (is.null(memberList$geo_x)) memberList$geo_x <- NA_real_
  if (is.null(memberList$geo_y)) memberList$geo_y <- NA_real_

  # Flag address-valid rows that still failed to geocode (no candidate returned).
  failed_geo <- valid_addresses & (is.na(memberList$geo_x) | is.na(memberList$geo_y))
  memberList$geocode_status[failed_geo] <- "No geocode match"

  if (verbose) {
    cat(paste("Number of geocoded addresses:",
              sum(!is.na(memberList$geo_x) & !is.na(memberList$geo_y)), "\n"))
  }

  # Invariant: geocoding never adds, drops, or reorders member rows -- every
  # input row is returned at its original position, matched to its result or NA.
  # Also, every "OK" row must carry coordinates and every non-"OK" row must not,
  # so the status flag and the data can never disagree.
  stopifnot(
    nrow(memberList) == n_input,
    identical(memberList$original_row_id, original_row_id_before),
    all((memberList$geocode_status == "OK") ==
          (!is.na(memberList$geo_x) & !is.na(memberList$geo_y)))
  )

  memberList
}


# ---------------------------------------------------------------------------
# Assigning points to districts
# ---------------------------------------------------------------------------

# Collapse a spatial-join result back to one row per input point. st_join emits
# one row per polygon a point falls inside, so a point landing in overlapping
# polygons would multiply into several rows (and compound across layers). We keep
# the FIRST match per id and note how many points were affected.
keep_first_match <- function(joined, id_col = "original_row_id", verbose = TRUE) {
  dup <- duplicated(joined[[id_col]])
  if (any(dup) && verbose) {
    n_multi <- length(unique(joined[[id_col]][dup]))
    message(sprintf(
      "%d point(s) fell within multiple polygons in a layer; keeping the first match.",
      n_multi
    ))
  }
  joined[!dup, , drop = FALSE]
}

# Assign already-geocoded points to one or more PREPARED district layers.
#
# geocoded : output of GeocodeMembers() (all rows, with x/y and geocode_status).
# layers   : list of prepared sf layers (see prepare_district_layers()). This
#            function does NOT prepare/prefix them, so callers reusing geocoded
#            points across many layer sets pay preparation only once.
# removeGEO: drop the geometry column from the result (TRUE = clean CSV export).
#
# Every input row is carried through: rows without coordinates become empty
# points and simply receive NA district columns. Supports any number of layers.
AssignToDistricts <- function(geocoded, layers, removeGEO = TRUE, verbose = TRUE) {
  if (!inherits(layers, "list")) layers <- list(layers)

  target_crs <- sf::st_crs(layers[[1]])

  # Turn EVERY row into a point; NA coordinates become empty points (na.fail =
  # FALSE) that a left st_join keeps with NA district columns.
  member_pts <- geocoded |>
    sf::st_as_sf(
      coords = c("geo_x", "geo_y"),
      crs = sf::st_crs("EPSG:4326"),
      na.fail = FALSE
    ) |>
    sf::st_transform(crs = target_crs)

  # Join each layer in turn, de-duping to first match after each so overlapping
  # polygons can't multiply rows (and can't compound across layers).
  final <- member_pts
  for (i in seq_along(layers)) {
    layer <- sf::st_transform(layers[[i]], crs = target_crs)
    final <- sf::st_join(final, layer, join = sf::st_within)
    final <- keep_first_match(final, "original_row_id", verbose = verbose)
  }

  # Invariant: after joining + first-match de-dup, the result has exactly one row
  # per input member, in the original order. Assignment never loses or multiplies
  # a member row.
  stopifnot(
    nrow(final) == nrow(geocoded),
    identical(final$original_row_id, geocoded$original_row_id)
  )

  result <- if (isTRUE(removeGEO)) sf::st_drop_geometry(final) else final

  # Summary counts come from the geocode_status flags on the (row-complete)
  # input, so they reflect input rows regardless of join behavior.
  invalid_count <- sum(geocoded$geocode_status == "Missing address")
  failed_count  <- sum(geocoded$geocode_status == "No geocode match")
  success_count <- sum(geocoded$geocode_status == "OK")

  status_cols <- c("original_row_id", "Street.Address", "City", "geocode_status")
  invalid_rows <- geocoded[geocoded$geocode_status == "Missing address", status_cols]
  failed_rows  <- geocoded[geocoded$geocode_status == "No geocode match", status_cols]

  attr(result, "summary_stats") <- list(
    original_count = nrow(geocoded),
    invalid_address_count = invalid_count,
    invalid_rows = if (invalid_count > 0) invalid_rows else NULL,
    failed_geocoding_count = failed_count,
    failed_geocoding_rows = if (failed_count > 0) failed_rows else NULL,
    successful_count = success_count
  )

  result
}

# Print the summary statistics attached to an AssignToDistricts()/AssignDistricts()
# result to the console.
print_summary <- function(result) {
  stats <- attr(result, "summary_stats")
  if (is.null(stats)) return(invisible(NULL))

  cat("\n=== SUMMARY ===\n")
  cat(sprintf("Total addresses: %d\n", stats$original_count))
  cat(sprintf("Invalid addresses (blank/NA): %d\n", stats$invalid_address_count))
  cat(sprintf("Failed geocoding: %d\n", stats$failed_geocoding_count))
  cat(sprintf("Successfully geocoded: %d\n", stats$successful_count))

  if (!is.null(stats$failed_geocoding_rows)) {
    cat("\nFailed addresses:\n")
    fr <- stats$failed_geocoding_rows
    for (i in seq_len(nrow(fr))) {
      cat(sprintf("  Row %d: %s, %s\n",
                  fr$original_row_id[i], fr$Street.Address[i], fr$City[i]))
    }
  }
  cat("===============\n\n")
  invisible(NULL)
}


# ---------------------------------------------------------------------------
# One-shot convenience wrapper
# ---------------------------------------------------------------------------

# Geocode a member list and assign it to one or more district layers in a single
# call. Convenient for the command-line workflow. For the "geocode once, apply to
# many districting files" workflow, call GeocodeMembers() once and then
# AssignToDistricts() repeatedly instead.
#
# memberList    : data.frame or path to a CSV of addresses.
# StreetCol     : name of the street-address column.
# CityCol       : name of the city column.
# districtsList : a single layer/path or an (optionally named) list of them.
# removeGEO     : drop the geometry column from the result (default TRUE).
# districtNames : optional names, one per layer, used to prefix output columns.
# restrictToDistrictArea : if TRUE, geocoding is limited to the (padded) union
#                 extent of the district layers. Off by default -- see the note
#                 on compute_search_extent() for why. Ignored if `boundaries` is
#                 supplied.
# boundaries    : optional explicit geocoding search extent (a bbox). Overrides
#                 restrictToDistrictArea. Default NULL = no restriction.
# verbose       : print progress and a summary to the console.
AssignDistricts <- function(memberList, StreetCol = "Street.Address", CityCol = "City",
                            districtsList, removeGEO = TRUE, districtNames = NULL,
                            restrictToDistrictArea = FALSE, boundaries = NULL,
                            verbose = TRUE) {
  layers <- prepare_district_layers(districtsList, districtNames)

  # Geocoding is unrestricted by default; only build a search extent when the
  # caller explicitly asks for one (or supplies their own).
  if (is.null(boundaries) && isTRUE(restrictToDistrictArea)) {
    boundaries <- compute_search_extent(layers)
  }

  geocoded <- GeocodeMembers(memberList, StreetCol, CityCol, boundaries, verbose = verbose)
  result <- AssignToDistricts(geocoded, layers, removeGEO = removeGEO, verbose = verbose)

  if (verbose) print_summary(result)

  result
}
