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
# Modified for Shiny App
AssignDistricts <- function(memberList, StreetCol, CityCol, districts, extraDistricts = NULL, removeGEO = TRUE){
  # Use the selected columns for Street.Address and City
  memberList$Street.Address <- memberList[[StreetCol]]
  memberList$City <- memberList[[CityCol]]
  
  # Remove rows with NA or blank in Street.Address or City column
  memberList <- memberList[!is.na(memberList$Street.Address) & 
                             memberList$Street.Address != "" &
                             !is.na(memberList$City) &
                             memberList$City != "", ]
  
  print(paste("Number of valid addresses:", nrow(memberList)))
  
  boundaries <- sf::st_bbox(districts)
  
  memberGeos <- arcgisgeocode::find_address_candidates(
    address = memberList$Street.Address,
    city = memberList$City,
    search_extent = boundaries,
    max_locations = 1
  )
  
  print(paste("Number of geocoded results:", nrow(memberGeos)))
  
  # Force binding columns if row counts match
  if (nrow(memberList) == nrow(memberGeos)) {
    combo_df <- data.frame(memberList, memberGeos)
  } else {
    warning("Number of geocoded results doesn't match input addresses. Attempting to join by address.")
    combo_df <- dplyr::bind_cols(memberList, memberGeos)
  }
  
  # Remove rows with NA in x or y coordinates
  combo_df <- combo_df[!is.na(combo_df$x) & !is.na(combo_df$y), ]
  
  print(paste("Number of valid geocoded addresses:", nrow(combo_df)))
  
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
    return(sf::read_sf(shp_files[1]))
  } else if (length(geojson_files) > 0) {
    return(yyjsonr::read_geojson_file(geojson_files[1]))
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
      fileInput("districts", "Upload Districts ZIP (Must Contain SHP or GeoJSON)", 
                accept = ".zip"),
      fileInput("extraDistricts", "Upload Extra Districts ZIP (Optional, Must contain SHP or GeoJSON)", 
                accept = ".zip"),
      checkboxInput("removeGEO", "Remove Geometry Column", value = TRUE),
      actionButton("run", "Assign Districts"),
      textOutput("downloadInstructions"),
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
      
      districts <- tryCatch({
        unzip_and_read_spatial(input$districts$datapath)
      }, error = function(e) {
        stop(paste("Error reading districts file:", e$message))
      })
      
      extraDistricts <- if (!is.null(input$extraDistricts)) {
        tryCatch({
          unzip_and_read_spatial(input$extraDistricts$datapath)
        }, error = function(e) {
          warning(paste("Error reading extra districts file:", e$message))
          NULL
        })
      } else NULL
      
      result <- tryCatch({
        AssignDistricts(memberList, districts, extraDistricts, input$removeGEO, 
                        StreetCol = input$streetColumn, CityCol = input$cityColumn)
      }, error = function(e) {
        print(paste("Error in AssignDistricts:", e$message))
        print("memberList structure:")
        print(str(memberList))
        print("districts structure:")
        print(str(districts))
        if (!is.null(extraDistricts)) {
          print("extraDistricts structure:")
          print(str(extraDistricts))
        }
        stop(e)
      })
      
      results(result)
    }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error")
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
  
  output$downloadInstructions <- renderText({
    if (is.null(input$selectedColumns) || length(input$selectedColumns) == 0) {
      "Please select at least one column to download."
    } else {
      paste("Selected columns:", length(input$selectedColumns))
    }
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("assigned_districts_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      data_to_write <- results()
      
      # Filter columns based on user selection
      selected_cols <- input$selectedColumns
      if (!is.null(selected_cols) && length(selected_cols) > 0) {
        data_to_write <- data_to_write[, selected_cols, drop = FALSE]
        write.csv(data_to_write, file, row.names = FALSE)
      } else {
        stop("Please select at least one column to download.")
      }
    }
  )
}

shinyApp(ui, server)
