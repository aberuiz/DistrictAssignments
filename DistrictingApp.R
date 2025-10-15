#install.packages(c("shiny","DT","bslib","sf","arcgisgeocode","yyjsonr","dplyr","gdalraster"))
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

# AssignDistricts function
# Modified for Shiny App - supports multiple district layers
AssignDistricts <- function(memberList, StreetCol, CityCol, districtsList, removeGEO = TRUE, progress = NULL){
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

  print(paste("Number of valid addresses:", nrow(memberList)))

  # Ensure all districts have valid geometry and use the first one for boundaries
  districtsList <- lapply(districtsList, sf::st_make_valid)

  boundaries <- sf::st_bbox(districtsList[[1]])

  # Update progress before geocoding
  if (!is.null(progress)) {
    progress$set(message = "Geocoding addresses...", value = 0.3)
  }

  memberGeos <- arcgisgeocode::find_address_candidates(
    address = memberList$Street.Address,
    city = memberList$City,
    search_extent = boundaries,
    max_locations = 1
  )

  # Update progress after geocoding
  if (!is.null(progress)) {
    progress$set(message = "Processing results...", value = 0.6)
  }

  print(paste("Number of geocoded results:", nrow(memberGeos)))

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

  print(paste("Number of valid geocoded addresses:", nrow(combo_df)))

  # Update progress before spatial joins
  if (!is.null(progress)) {
    progress$set(message = "Performing spatial joins...", value = 0.8)
  }

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

  # Store statistics as attributes
  result <- if (isTRUE(removeGEO)) {
    sf::st_drop_geometry(final)
  } else {
    final
  }

  # Attach summary statistics
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

# Function to unzip and read spatial file
unzip_and_read_spatial <- function(zipfile) {
  temp_dir <- tempdir()
  utils::unzip(zipfile, exdir = temp_dir)
  
  # Search for .shp files recursively
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

      progress$set(message = "Starting address assignment...", value = 0.2)

      result <- tryCatch({
        AssignDistricts(memberList,
                        StreetCol = input$streetColumn,
                        CityCol = input$cityColumn,
                        districtsList = districtsList,
                        removeGEO = input$removeGEO,
                        progress = progress)
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
      result_data <- result_data[, sapply(result_data, is.atomic)]
      max_length <- max(sapply(result_data, length))
      result_data <- as.data.frame(lapply(result_data, function(x) {
        length(x) <- max_length
        x
      }))
      
      datatable(result_data, options = list(scrollX = TRUE, pageLength = 5))
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
