#install.packages(c("sf","arcgisgeocode","yyjsonr","dplyr","gdalraster"))
library(sf)
library(arcgisgeocode)
library(yyjsonr)
library(dplyr)
library(gdalraster)

AssignDistricts <- function(memberList, StreetCol="Street.Address", CityCol="City", districtsList, removeGEO = TRUE){
  # Member List Path - read if it's a file path, otherwise use as-is
  if (is.character(memberList) && length(memberList) == 1) {
    memberList <- read.csv(memberList)
  }

  # Use the selected columns for Street.Address and City
  memberList$Street.Address <- memberList[[StreetCol]]
  memberList$City <- memberList[[CityCol]]

  # Store original row count and identifiers
  original_count <- nrow(memberList)
  memberList$original_row_id <- seq_len(nrow(memberList))

  # Remove rows with NA or blank in Street.Address or City column
  valid_addresses <- !is.na(memberList$Street.Address) &
                     memberList$Street.Address != "" &
                     !is.na(memberList$City) &
                     memberList$City != ""

  invalid_rows <- memberList[!valid_addresses, ]
  memberList <- memberList[valid_addresses, ]

  cat(paste("Number of valid addresses:", nrow(memberList), "\n"))

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

  # Convert districtsList to a list if needed
  if (!is.list(districtsList)) {
    districtsList <- list(districtsList)
  }

  # Read spatial files if they're file paths
  for (i in seq_along(districtsList)) {
    if (is.character(districtsList[[i]]) && length(districtsList[[i]]) == 1) {
      districtsList[[i]] <- read_spatial_file(districtsList[[i]])
    }
  }

  # Ensure all districts have valid geometry
  districtsList <- lapply(districtsList, sf::st_make_valid)

  # BBOX from first district
  boundaries <- sf::st_bbox(districtsList[[1]])

  # Geocode Member List
  cat("Geocoding addresses...\n")
  memberGeos <- arcgisgeocode::find_address_candidates(
    address = memberList$Street.Address,
    city = memberList$City,
    search_extent = boundaries,
    max_locations = 1
  )

  cat(paste("Number of geocoded results:", nrow(memberGeos), "\n"))

  # Strict validation: row counts MUST match
  if (nrow(memberList) != nrow(memberGeos)) {
    stop(paste("Geocoding error: Expected", nrow(memberList),
               "results but got", nrow(memberGeos),
               "This indicates a problem with the geocoding service."))
  }

  combo_df <- data.frame(memberList, memberGeos)

  # Identify rows with failed geocoding (NA coordinates)
  failed_geocoding <- is.na(combo_df$x) | is.na(combo_df$y)
  failed_rows <- combo_df[failed_geocoding, c("original_row_id", "Street.Address", "City")]

  # Remove rows with NA in x or y coordinates
  combo_df <- combo_df[!failed_geocoding, ]

  cat(paste("Number of valid geocoded addresses:", nrow(combo_df), "\n"))

  # Create pts
  member_pts <- combo_df |>
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

  # Store statistics
  result <- if (isTRUE(removeGEO)) {
    sf::st_drop_geometry(final)
  } else {
    final
  }

  # Print summary statistics
  cat("\n=== SUMMARY ===\n")
  cat(sprintf("Total addresses: %d\n", original_count))
  cat(sprintf("Invalid addresses (blank/NA): %d\n", nrow(invalid_rows)))
  cat(sprintf("Failed geocoding: %d\n", nrow(failed_rows)))
  cat(sprintf("Successfully assigned: %d\n", nrow(result)))

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
    invalid_address_count = nrow(invalid_rows),
    invalid_rows = if(nrow(invalid_rows) > 0) invalid_rows else NULL,
    failed_geocoding_count = nrow(failed_rows),
    failed_geocoding_rows = if(nrow(failed_rows) > 0) failed_rows else NULL,
    successful_count = nrow(result)
  )

  return(result)
}