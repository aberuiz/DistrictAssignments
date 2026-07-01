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

# Shared core logic (read/prepare layers, geocode, assign). Keeping it in one
# file means the app and the command-line script can't drift apart. Sourced
# relative to the app, so run the app from the repository root
# (e.g. shiny::runApp("DistrictingApp.R")).
source("districting_core.R")

# Shiny UI
ui <- fluidPage(
  titlePanel("Assign Districts"),
  sidebarLayout(
    sidebarPanel(
      fileInput("memberList", "Upload Member List (CSV)", accept = ".csv"),
      uiOutput("columnSelection"),
      uiOutput("geocodeUI"),
      textOutput("geocodeStatus"),
      tags$hr(),
      fileInput("districts", "District Files (ZIP containing SHP or GeoJSON)",
                accept = ".zip",
                multiple = TRUE,
                buttonLabel = "Add File(s)..."),
      helpText("You can keep adding district files. At least one is required."),
      uiOutput("districtNaming"),
      uiOutput("clearDistrictsUI"),
      checkboxInput("removeGEO", "Remove Geometry Column", value = TRUE),
      checkboxInput("restrictGeo", "Restrict geocoding to district area", value = FALSE),
      actionButton("run", "Assign Districts"),
      actionButton("regeocode", "Clear geocode cache"),
      helpText("Clear the cache to force a fresh geocode."),
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

  # Accumulated district files. A single fileInput only keeps the most recent
  # selection, so we copy each upload to a stable per-session directory and
  # append it here. This lets the user add district files incrementally (upload
  # some, then add more) rather than picking them all in one dialog.
  # Holds a data.frame with columns: name, datapath.
  districtFiles <- reactiveVal(NULL)

  # Geocode cache: geocoding is the slow, rate-limited step, so we run it once
  # per unique set of address/city values and reuse the points when only the
  # district files change. This is what lets you geocode once and then apply many
  # districting files. `geocodeKey` holds the address/city data the cache was
  # built from; when it changes, we re-geocode.
  geocodedData <- reactiveVal(NULL)
  geocodeKey <- reactiveVal(NULL)

  # Append each new upload to the accumulated list, copying to a persistent
  # directory so later uploads can't invalidate earlier temp files.
  observeEvent(input$districts, {
    req(input$districts)
    persist_dir <- file.path(tempdir(), "district_uploads")
    if (!dir.exists(persist_dir)) dir.create(persist_dir)

    stored <- vapply(seq_len(nrow(input$districts)), function(i) {
      dest <- tempfile("district_", tmpdir = persist_dir, fileext = ".zip")
      file.copy(input$districts$datapath[i], dest)
      dest
    }, character(1))

    new_files <- data.frame(
      name = input$districts$name,
      datapath = stored,
      stringsAsFactors = FALSE
    )
    districtFiles(rbind(districtFiles(), new_files))
  })

  # Clear the accumulated district files.
  observeEvent(input$clearDistricts, {
    districtFiles(NULL)
    showNotification("Cleared district files.", type = "message", duration = 3)
  })

  # Show a "Clear district files" button only once files have been added.
  output$clearDistrictsUI <- renderUI({
    req(districtFiles())
    actionButton("clearDistricts", "Clear district files")
  })

  # Read + validate + name-prefix all accumulated district layers. Called from
  # both the geocode (when restricting) and assign handlers, so the reading and
  # naming logic lives in one place.
  read_prepared_layers <- function() {
    files <- districtFiles()
    if (is.null(files) || nrow(files) == 0) {
      stop("At least one district file is required")
    }
    districtsList <- lapply(seq_len(nrow(files)), function(i) {
      tryCatch(
        unzip_and_read_spatial(files$datapath[i]),
        error = function(e) stop(paste("Error reading district file", i, ":", e$message))
      )
    })
    districtNames <- vapply(seq_len(nrow(files)), function(i) {
      nm <- input[[paste0("districtName_", i)]]
      if (is.null(nm) || !nzchar(nm)) tools::file_path_sans_ext(files$name[i]) else nm
    }, character(1))
    prepare_district_layers(districtsList, districtNames)
  }

  # A stable, comparable signature for the search extent. Two freshly-computed
  # bboxes for the same area can differ in attributes or floating-point detail,
  # which would otherwise bust the cache; rounding the plain coordinates makes
  # the comparison robust. NULL (unrestricted) compares equal to NULL.
  bbox_signature <- function(b) if (is.null(b)) NULL else round(as.numeric(b), 6)

  # Return the cached geocode if it still matches the current inputs, otherwise
  # geocode and cache. Centralizes the "geocode once, reuse" logic so both the
  # standalone Geocode button and the Assign button share it -- which is what lets
  # Assign accept the geocoding the Geocode button already did. The cache key is
  # the address/city values plus the (normalized) search extent.
  ensure_geocoded <- function(memberList, streetCol, cityCol, boundaries, progress = NULL) {
    key <- list(
      street = memberList[[streetCol]],
      city   = memberList[[cityCol]],
      bbox   = bbox_signature(boundaries)
    )
    if (!is.null(geocodedData()) && identical(key, geocodeKey())) {
      showNotification("Reusing existing geocoding.", type = "message", duration = 2)
      return(geocodedData())
    }
    if (!is.null(progress)) progress$set(message = "Geocoding addresses...", value = 0.3)
    geocoded <- GeocodeMembers(memberList, streetCol, cityCol, boundaries = boundaries)
    geocodedData(geocoded)
    geocodeKey(key)
    geocoded
  }

  # Geocode button appears once a CSV and its address/city columns are chosen.
  # Geocoding is independent of the district files (unless "restrict" is on), so
  # the user can start this slow step early while still gathering district files.
  output$geocodeUI <- renderUI({
    req(csvData(), input$streetColumn, input$cityColumn)
    actionButton("geocode", "Geocode Addresses")
  })

  # Geocode on demand, independent of district assignment.
  observeEvent(input$geocode, {
    req(input$memberList, input$streetColumn, input$cityColumn)
    tryCatch({
      memberList <- csvData()

      progress <- Progress$new()
      on.exit(progress$close())
      progress$set(message = "Preparing to geocode...", value = 0.1)

      # Only restrict to a district extent when asked AND files are available;
      # otherwise geocode unrestricted so this can run before districts exist.
      boundaries <- NULL
      if (isTRUE(input$restrictGeo)) {
        files <- districtFiles()
        if (is.null(files) || nrow(files) == 0) {
          showNotification(
            "Restrict is on but no district files added yet; geocoding without a restriction.",
            type = "warning", duration = 5
          )
        } else {
          boundaries <- compute_search_extent(read_prepared_layers())
        }
      }

      ensure_geocoded(memberList, input$streetColumn, input$cityColumn, boundaries, progress)
      progress$set(message = "Complete!", value = 1)
      showNotification("Geocoding complete.", type = "message", duration = 3)
    }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error", duration = NULL)
      print(paste("Geocode error:", e))
    })
  })

  # Live geocoding status shown under the Geocode button.
  output$geocodeStatus <- renderText({
    g <- geocodedData()
    if (is.null(g)) return("Addresses not geocoded yet.")
    n_ok      <- sum(g$geocode_status == "OK")
    n_fail    <- sum(g$geocode_status == "No geocode match")
    n_missing <- sum(g$geocode_status == "Missing address")
    sprintf("Geocoded %d of %d addresses (%d no match, %d missing address).",
            n_ok, nrow(g), n_fail, n_missing)
  })

  observeEvent(input$memberList, {
    req(input$memberList)
    csvData(read.csv(input$memberList$datapath))
    # A new CSV invalidates any cached geocode.
    geocodedData(NULL)
    geocodeKey(NULL)
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

  # One text input per accumulated district file, letting the user name each
  # layer. The name becomes the prefix on that layer's output columns (e.g. a
  # name of "Congressional" turns a "DISTRICT" column into
  # "Congressional_DISTRICT"). Defaults to the uploaded file's base name.
  #
  # This UI re-renders whenever a file is added, which recreates the text boxes.
  # We isolate() the current input values and reuse them as defaults so any names
  # the user already typed are preserved instead of reverting to the file name.
  output$districtNaming <- renderUI({
    req(districtFiles())
    files <- districtFiles()
    isolate({
      lapply(seq_len(nrow(files)), function(i) {
        current <- input[[paste0("districtName_", i)]]
        default <- if (!is.null(current) && nzchar(current)) {
          current
        } else {
          tools::file_path_sans_ext(files$name[i])
        }
        textInput(
          inputId = paste0("districtName_", i),
          label   = paste("Name for district file", i),
          value   = default
        )
      })
    })
  })
  
  # Manually clear the geocode cache so the next run geocodes from scratch.
  observeEvent(input$regeocode, {
    geocodedData(NULL)
    geocodeKey(NULL)
    showNotification("Geocode cache cleared. The next run will re-geocode.",
                     type = "message", duration = 4)
  })

  observeEvent(input$run, {
    req(input$memberList, input$streetColumn, input$cityColumn)

    tryCatch({
      memberList <- csvData()

      # Create a progress indicator
      progress <- Progress$new()
      on.exit(progress$close())
      progress$set(message = "Loading district files...", value = 0.1)

      # Read, validate and name-prefix the accumulated district layers.
      layers <- read_prepared_layers()
      print(paste("Number of district files loaded:", length(layers)))

      # Optional geocoding restriction: only build a search extent when the user
      # asks for it (off by default). See compute_search_extent() for rationale.
      boundaries <- if (isTRUE(input$restrictGeo)) compute_search_extent(layers) else NULL

      # Geocode once and reuse: if the user already pressed "Geocode Addresses"
      # (or ran before) with the same address/city + extent, this returns the
      # cached points instead of geocoding again.
      geocoded <- ensure_geocoded(memberList, input$streetColumn, input$cityColumn,
                                  boundaries, progress)

      # --- Assign to districts ---------------------------------------------
      progress$set(message = "Assigning districts...", value = 0.7)
      result <- tryCatch({
        AssignToDistricts(geocoded, layers, removeGEO = input$removeGEO)
      }, error = function(e) {
        print(paste("Error in AssignToDistricts:", e$message))
        print(str(geocoded))
        stop(e)
      })

      progress$set(message = "Complete!", value = 1)

      results(result)

      # Extract and display summary statistics
      stats <- attr(result, "summary_stats")
      if (!is.null(stats)) {
        # Build summary message
        summary_msg <- sprintf(
          "Processing complete!\n\nTotal addresses: %d\nInvalid addresses (blank/NA): %d\nFailed geocoding: %d\nSuccessfully geocoded: %d",
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
