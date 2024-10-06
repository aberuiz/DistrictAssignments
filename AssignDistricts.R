#install.packages(c("sf","arcgisgeocode","yyjsonr","dplyr"))
library(sf)
library(arcgisgeocode)
library(yyjsonr)
library(dplyr)

AssignDistricts <- function(memberList, districts, extraDistricts = NULL, removeGEO = TRUE){
  # Member List Path
  memberList <- read.csv(memberList)
  
  # Remove rows with NA or blank in Street.Address column
  memberList <- memberList[!is.na(memberList$Street.Address) & memberList$Street.Address != "", ]

  read_spatial_file <- function(file_path) {
    # Get the file extension
    file_ext <- tolower(tools::file_ext(file_path))

    # Make sure only 1 file is read
    file_ext <- tolower(tools::file_ext(file_path))
    if (length(file_ext) != 1) {
      stop("Invalid file extension")
    }
    
    # Read the file based on its extension
    spatial_object <- switch(
      file_ext,
      "shp" = {
       sf::st_read(file_path) |>
         sf::st_make_valid()
      },
      "geojson" = {
       yyjsonr::read_geojson_file(file_path)
      },
      {
       stop("Unsupported file format. Please provide a .shp or .geojson file.")
      }
    )

    return(spatial_object)
  }

  districts <- read_spatial_file(districts)

  if (!is.null(extraDistricts)) {
    tryCatch({
      extraDistricts <- read_spatial_file(extraDistricts)
    }, error = function(e) {
      warning("Error reading extraDistricts file: ", e$message)
      extraDistricts <- NULL
    })
  } else {
    extraDistricts <- NULL
  }


  # BBOX
  boundaries <- sf::st_bbox(districts)

  # Geocode Member List
  memberGeos <- arcgisgeocode::find_address_candidates(
    address = memberList$Street.Address,
    city = memberList$City,
    search_extent = boundaries,
    max_locations = 1
  )

  # Join memberList & memberGeo Results
  if (NROW(memberList) != NROW(memberGeos)){
    stop(message("geocoding error"))
  } else combo_df <- dplyr::bind_cols(memberList, memberGeos)

  # Create pts
  member_pts <- combo_df |>
    sf::st_as_sf(
      coords = c("x","y"),
      crs = sf::st_crs("EPSG:4326"),
      na.fail = FALSE
    ) |>
    sf::st_transform(crs = sf::st_crs(districts))

  final <- sf::st_join(member_pts, districts, join = sf::st_within)

  if (is.null(extraDistricts)) {
    if (isTRUE(removeGEO)) {
      return(sf::st_drop_geometry(final))
    } else {
      return(final)
    }
  } else {
    extraDistricts <- extraDistricts |>
      sf::st_transform(crs = sf::st_crs(districts))
    extra_final <- sf::st_join(final, extraDistricts, join = sf::st_within)
    
    if (isTRUE(removeGEO)) {
      return(sf::st_drop_geometry(extra_final))
    } else {
      return(extra_final)
    }
  }
}
