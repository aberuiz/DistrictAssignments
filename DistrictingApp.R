# install pak if missing
if (!requireNamespace("pak", quietly = TRUE)) {
  install.packages("pak", repos = "https://cloud.r-project.org")
}

# Install any missing packages needed
pak::pak(c("shiny", "DT", "bslib", "sf", "arcgisgeocode", "yyjsonr", "dplyr", "gdalraster"))

library(shiny)
library(DT)
library(bslib)
library(sf)
library(arcgisgeocode)
library(yyjsonr)
library(dplyr)
library(gdalraster)

# Increase the maximum file upload size to 20MB
options(shiny.maxRequestSize = 20 * 1024^2)

# ---------------------------------------------------------------------------
# District layer naming helpers (kept in sync with AssignDistricts.R)
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
# In the Shiny app the layers arrive already-read (as sf objects), so names come
# from districtNames (supplied from the per-file UI text inputs); when absent a
# positional fallback of "District_1", "District_2", ... is used.
resolve_layer_names <- function(districtsList, districtNames = NULL) {
  list_names <- names(districtsList)
  vapply(seq_along(districtsList), function(i) {
    if (!is.null(districtNames) && length(districtNames) >= i && nzchar(districtNames[i])) {
      return(districtNames[i])
    }
    if (!is.null(list_names) && nzchar(list_names[i])) {
      return(list_names[i])
    }
    paste0("District_", i)
  }, character(1))
}

# AssignDistricts() -- Shiny variant
# Geocodes a member list and assigns each address to the district(s) it falls
# within, across one or more district layers, reporting progress to the UI.
#
# Arguments:
#   memberList    - data.frame of addresses (already loaded from the CSV upload).
#   StreetCol     - name of the street-address column in memberList.
#   CityCol       - name of the city column in memberList.
#   districtsList - list of already-read district sf layers.
#   removeGEO     - if TRUE (default) the geometry column is dropped from output.
#   progress      - optional shiny Progress object for UI status updates.
#   districtNames - optional character vector, one name per district layer, used
#                   to prefix each layer's columns so they stay distinguishable.
AssignDistricts <- function(memberList, StreetCol, CityCol, districtsList, removeGEO = TRUE, progress = NULL, districtNames = NULL){
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

  print(paste("Number of geocodable addresses:", sum(valid_addresses)))

  # Ensure all districts have valid geometry and use the first one for boundaries
  districtsList <- lapply(districtsList, sf::st_make_valid)

  # Prefix each layer's attribute columns with its resolved name so columns from
  # different layers stay distinct in the joined output (see #4).
  layer_names <- resolve_layer_names(districtsList, districtNames)
  districtsList <- Map(prefix_layer_columns, districtsList, layer_names)

  boundaries <- sf::st_bbox(districtsList[[1]])

  # Update progress before geocoding
  if (!is.null(progress)) {
    progress$set(message = "Geocoding addresses...", value = 0.3)
  }

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
    memberGeos <- arcgisgeocode::find_address_candidates(
      address = memberList$Street.Address[valid_idx],
      city = memberList$City[valid_idx],
      search_extent = boundaries,
      max_locations = 1
    )
    print(paste("Number of geocoded results:", nrow(memberGeos)))

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
    print("No geocodable addresses found.")
  }

  # Update progress after geocoding
  if (!is.null(progress)) {
    progress$set(message = "Processing results...", value = 0.6)
  }

  # Guarantee x/y coordinate columns exist even if geocoding produced none.
  if (is.null(memberList$x)) memberList$x <- NA_real_
  if (is.null(memberList$y)) memberList$y <- NA_real_

  # Flag address-valid rows that still failed to geocode (no candidate returned).
  failed_geo <- valid_addresses & (is.na(memberList$x) | is.na(memberList$y))
  memberList$geocode_status[failed_geo] <- "No geocode match"

  print(paste("Number of geocoded addresses:",
              sum(!is.na(memberList$x) & !is.na(memberList$y))))

  # Update progress before spatial joins
  if (!is.null(progress)) {
    progress$set(message = "Performing spatial joins...", value = 0.8)
  }

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
  # Counts come from the flag column on the (row-complete) member list, so they
  # reflect input rows regardless of any join row-multiplication.
  invalid_count <- sum(memberList$geocode_status == "Missing address")
  failed_count  <- sum(memberList$geocode_status == "No geocode match")
  success_count <- sum(memberList$geocode_status == "OK")

  status_cols <- c("original_row_id", "Street.Address", "City", "geocode_status")
  invalid_rows <- memberList[memberList$geocode_status == "Missing address", status_cols]
  failed_rows  <- memberList[memberList$geocode_status == "No geocode match", status_cols]

  # Attach summary statistics
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

# Unzip a single uploaded district archive and read the spatial layer it contains.
#
# IMPORTANT: each call extracts into its OWN unique directory (via tempfile()).
# Previously this used the shared session tempdir() for every call, so when a
# user uploaded multiple district zips they were all extracted side-by-side into
# the same folder. The subsequent recursive list.files(...)[1] then matched the
# FIRST .shp/.geojson from *any* archive, causing every uploaded file to resolve
# to the same layer. Isolating each archive in its own directory guarantees the
# glob only sees the file(s) from the archive currently being read.
#
# Returns: an sf object with validated, repaired geometry.
unzip_and_read_spatial <- function(zipfile) {
  # Unique, per-archive extraction directory (prevents cross-archive contamination)
  temp_dir <- tempfile("district_")
  dir.create(temp_dir)
  utils::unzip(zipfile, exdir = temp_dir)

  # Search this archive's directory only for a supported spatial file
  shp_files <- list.files(temp_dir, pattern = "\\.shp$", full.names = TRUE, recursive = TRUE)
  geojson_files <- list.files(temp_dir, pattern = "\\.geojson$", full.names = TRUE, recursive = TRUE)

  if (length(shp_files) > 0) {
    # If multiple .shp files are found, use the first one
    sf::sf_use_s2(FALSE)
    # Set GDAL configuration option
    old_config <- gdalraster::get_config_option("SHAPE_RESTORE_SHX")
    gdalraster::set_config_option("SHAPE_RESTORE_SHX", "YES")
    on.exit(gdalraster::set_config_option("SHAPE_RESTORE_SHX", old_config))

    # Read in shapefile for new validation procedure
    districts <- sf::read_sf(shp_files[1])
    # Make valid and remove any potential geometry problems
    districts <- sf::st_make_valid(districts)
    # Additional cleanup if needed
    districts <- sf::st_buffer(districts, 0)

    return(districts)
  } else if (length(geojson_files) > 0) {
    # Read GeoJSON and apply same validation as shapefiles
    districts <- yyjsonr::read_geojson_file(geojson_files[1])
    districts <- sf::st_make_valid(districts)
    districts <- sf::st_buffer(districts, 0)

    return(districts)
  } else {
    stop("No .shp or .geojson file found in the zip archive")
  }
}

# Shiny UI
ui <- fluidPage(
  titlePanel("Assign Districts"),
  sidebarLayout(
    sidebarPanel(
      fileInput("memberList", "Upload Member List (CSV)", accept = ".csv"),
      uiOutput("columnSelection"),
      fileInput("districts", "Upload District Files (Must Contain SHP or GeoJSON)",
                accept = ".zip",
                multiple = TRUE),
      helpText("Upload one or more district ZIP files. At least one is required."),
      uiOutput("districtNaming"),
      checkboxInput("removeGEO", "Remove Geometry Column", value = TRUE),
      actionButton("run", "Assign Districts"),
      textOutput("downloadInstructions"),
      uiOutput("columnSelectionForDownload"),
      downloadButton("downloadData", "Download Selected Columns")
    ),
    mainPanel(
      DTOutput("results")
    )
  ),
  theme = bs_theme(version = 5, bootswatch = "minty")
)

server <- function(input, output, session) {
  
  csvData <- reactiveVal(NULL)
  results <- reactiveVal(NULL)
  
  observeEvent(input$memberList, {
    req(input$memberList)
    csvData(read.csv(input$memberList$datapath))
  })
  
  output$columnSelection <- renderUI({
    req(csvData())
    columns <- colnames(csvData())
    
    # Set default selections
    default_street <- if("Street.Address" %in% columns) "Street.Address" else columns[1]
    default_city <- if("City" %in% columns) "City" else columns[1]
    
    tagList(
      selectInput("streetColumn", "Select Street Address Column",
                  choices = columns,
                  selected = default_street),
      selectInput("cityColumn", "Select City Column",
                  choices = columns,
                  selected = default_city)
    )
  })

  # One text input per uploaded district file, letting the user name each layer.
  # The name becomes the prefix on that layer's output columns (e.g. a name of
  # "Congressional" turns a "DISTRICT" column into "Congressional_DISTRICT").
  # Defaults to the uploaded file's base name.
  output$districtNaming <- renderUI({
    req(input$districts)
    lapply(seq_len(nrow(input$districts)), function(i) {
      textInput(
        inputId = paste0("districtName_", i),
        label   = paste("Name for district file", i),
        value   = tools::file_path_sans_ext(input$districts$name[i])
      )
    })
  })
  
  observeEvent(input$run, {
    req(input$memberList, input$districts, input$streetColumn, input$cityColumn)

    tryCatch({
      memberList <- csvData()

      # Check that at least one district file was uploaded
      if (is.null(input$districts) || nrow(input$districts) == 0) {
        stop("At least one district file is required")
      }

      # Create a progress indicator
      progress <- Progress$new()
      on.exit(progress$close())
      progress$set(message = "Loading district files...", value = 0.1)

      # Read all district files into a list
      districtsList <- list()
      for (i in 1:nrow(input$districts)) {
        districtsList[[i]] <- tryCatch({
          unzip_and_read_spatial(input$districts$datapath[i])
        }, error = function(e) {
          stop(paste("Error reading district file", i, ":", e$message))
        })
      }

      print(paste("Number of district files loaded:", length(districtsList)))

      # Collect the per-file layer names from the naming UI, falling back to the
      # uploaded file's base name if a field is left blank.
      districtNames <- vapply(seq_len(nrow(input$districts)), function(i) {
        nm <- input[[paste0("districtName_", i)]]
        if (is.null(nm) || !nzchar(nm)) {
          tools::file_path_sans_ext(input$districts$name[i])
        } else {
          nm
        }
      }, character(1))

      progress$set(message = "Starting address assignment...", value = 0.2)

      result <- tryCatch({
        AssignDistricts(memberList,
                        StreetCol = input$streetColumn,
                        CityCol = input$cityColumn,
                        districtsList = districtsList,
                        removeGEO = input$removeGEO,
                        progress = progress,
                        districtNames = districtNames)
      }, error = function(e) {
        print(paste("Error in AssignDistricts:", e$message))
        print("memberList structure:")
        print(str(memberList))
        print("districtsList structure:")
        print(str(districtsList))
        stop(e)
      })

      progress$set(message = "Complete!", value = 1)

      results(result)

      # Extract and display summary statistics
      stats <- attr(result, "summary_stats")
      if (!is.null(stats)) {
        # Build summary message
        summary_msg <- sprintf(
          "Processing complete!\n\nTotal addresses: %d\nInvalid addresses (blank/NA): %d\nFailed geocoding: %d\nSuccessfully assigned: %d",
          stats$original_count,
          stats$invalid_address_count,
          stats$failed_geocoding_count,
          stats$successful_count
        )

        # Add details about failed rows if any
        if (stats$failed_geocoding_count > 0 && !is.null(stats$failed_geocoding_rows)) {
          failed_details <- paste(
            sprintf("Row %d: %s, %s",
                    stats$failed_geocoding_rows$original_row_id,
                    stats$failed_geocoding_rows$Street.Address,
                    stats$failed_geocoding_rows$City),
            collapse = "\n"
          )
          summary_msg <- paste(summary_msg, "\n\nFailed addresses:\n", failed_details, sep = "")
        }

        showNotification(summary_msg, type = "message", duration = NULL)
      }
    }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error", duration = NULL)
      # Log the full error for debugging
      print(paste("Full error:", e))
    })
  })
  
  output$results <- renderDT({
    req(results())
    result_data <- results()

    if (is.data.frame(result_data) && nrow(result_data) > 0) {
      # Keep only atomic columns (drops any sf geometry list-column). Rows are
      # now aligned by construction, so the old length-padding workaround is gone.
      result_data <- result_data[, sapply(result_data, is.atomic), drop = FALSE]

      dt <- datatable(result_data, options = list(scrollX = TRUE, pageLength = 5))

      # Highlight problem rows so they're visible instead of silently dropped:
      # anything not "OK" (missing address / no geocode match) gets a red tint.
      if ("geocode_status" %in% names(result_data)) {
        dt <- formatStyle(
          dt, "geocode_status",
          target = "row",
          backgroundColor = styleEqual("OK", "white", default = "#ffd9d9")
        )
      }
      dt
    } else {
      datatable(data.frame(Message = "No valid results to display"))
    }
  })
  
  availableColumns <- reactiveVal(NULL)
  
  observeEvent(results(), {
    req(results())
    availableColumns(colnames(results()))
  })
  
  # Render UI for column selection
  output$columnSelectionForDownload <- renderUI({
    req(availableColumns())
    checkboxGroupInput("selectedColumns", "Select columns to download:",
                       choices = availableColumns(),
                       selected = NULL)
  })
  
  # Update download instructions
  output$downloadInstructions <- renderText({
    if (is.null(input$selectedColumns) || length(input$selectedColumns) == 0) {
      "Please select at least one column to download."
    } else {
      paste("Selected columns:", length(input$selectedColumns))
    }
  })
  
  # Modify download handler
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("assigned_districts_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      req(results(), input$selectedColumns)
      data_to_write <- results()
      
      # Filter columns based on user selection
      selected_cols <- input$selectedColumns
      if (length(selected_cols) > 0) {
        data_to_write <- data_to_write[, selected_cols, drop = FALSE]
        write.csv(data_to_write, file, row.names = FALSE)
      } else {
        stop("Please select at least one column to download.")
      }
    }
  )
}

shinyApp(ui, server)
