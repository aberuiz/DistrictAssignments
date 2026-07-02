# Shiny app entry point. The app is a thin interface over the exported
# functions (prepare_district_layers, GeocodeMembers, AssignToDistricts), so
# the app and script workflows can't drift apart.

#' Launch the district-assignment Shiny app
#'
#' A point-and-click interface over [GeocodeMembers()] and
#' [AssignToDistricts()]: upload a member CSV, geocode it, add any number of
#' zipped district files (`.shp` or `.geojson`), and download the assigned
#' results. Requires the suggested packages `shiny`, `DT`, and `bslib`.
#'
#' @param max_upload_mb Maximum upload size in megabytes (default 20).
#' @param ... Passed on to [shiny::runApp()] (e.g. `port`, `launch.browser`).
#'
#' @return Runs the app; does not return while the app is live.
#'
#' @examples
#' \dontrun{
#' run_app()
#' }
#' @export
run_app <- function(max_upload_mb = 20, ...) {
  for (pkg in c("shiny", "DT", "bslib")) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(sprintf("Package '%s' is required to run the app. Install it with install.packages(\"%s\").",
                   pkg, pkg))
    }
  }

  old_max <- getOption("shiny.maxRequestSize")
  options(shiny.maxRequestSize = max_upload_mb * 1024^2)
  on.exit(options(shiny.maxRequestSize = old_max))

  shiny::runApp(shiny::shinyApp(app_ui(), app_server), ...)
}

app_ui <- function() {
  shiny::fluidPage(
    shiny::titlePanel("Assign Districts"),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::fileInput("memberList", "Upload Member List (CSV or Excel)",
                         accept = c(".csv", ".xlsx", ".xls")),
        shiny::uiOutput("columnSelection"),
        shiny::uiOutput("geocodeUI"),
        shiny::textOutput("geocodeStatus"),
        shiny::tags$hr(),
        shiny::fileInput("districts", "District Files (.gpkg, .geojson, or zipped .shp)",
                         accept = c(".zip", ".gpkg", ".geojson"),
                         multiple = TRUE,
                         buttonLabel = "Add File(s)..."),
        shiny::helpText(paste("You can keep adding district files. At least one is required.",
                              "Shapefiles must be zipped (with their sidecar files);",
                              "GeoPackage and GeoJSON can be uploaded directly or zipped.")),
        shiny::uiOutput("districtNaming"),
        shiny::uiOutput("clearDistrictsUI"),
        shiny::checkboxInput("removeGEO", "Remove Geometry Column", value = TRUE),
        shiny::checkboxInput("restrictGeo", "Restrict geocoding to district area", value = FALSE),
        shiny::actionButton("run", "Assign Districts"),
        shiny::actionButton("regeocode", "Clear geocode cache"),
        shiny::helpText("Clear the cache to force a fresh geocode."),
        shiny::textOutput("downloadInstructions"),
        shiny::uiOutput("columnSelectionForDownload"),
        shiny::downloadButton("downloadData", "Download Selected Columns")
      ),
      shiny::mainPanel(
        # FUTURE (results map): swap this for a tabsetPanel with a "Table" tab
        # (this DTOutput) and a "Map" tab -- an in-app tab, not a separate
        # browser window, so it needs no extra server or link plumbing.
        #
        # Map tab contents:
        #   - leaflet::leafletOutput("map") + one selectInput("mapLayer") that
        #     picks WHICH district layer drives the point colors. Rendering is
        #     lazy: leaflet only draws when the tab is opened.
        #   - Per-layer polygon groups via addPolygons(group = layer_name) and
        #     addLayersControl(overlayGroups = ...) give the built-in checkbox
        #     UI for switching layers on/off -- no custom code needed.
        #   - Per-district counts are one table() call on the layer's
        #     assignment column; show them as polygon labels/popups
        #     ("District 5 -- 123 members") and optionally a small summary
        #     table under the map. Counts come from the SAME results object the
        #     table shows, so they can never disagree.
        #   - Points: addCircleMarkers(lng = ~geo_x, lat = ~geo_y) colored by
        #     the selected layer's assignment; rows with NA coordinates can't
        #     be plotted, so list them in a caption ("4 rows not shown: ...").
        #   - Needs the geometry/coords: results already keep geo_x/geo_y even
        #     when removeGEO = TRUE, so no pipeline change is required.
        # Complexity containment: one tab + one dropdown, read-only (no
        # map-driven filtering/editing), leaflet in Suggests gated by
        # requireNamespace like DT/bslib. The base app gains no dependencies.
        DT::DTOutput("results")
      )
    ),
    theme = bslib::bs_theme(version = 5, bootswatch = "minty")
  )
}

app_server <- function(input, output, session) {

  csvData <- shiny::reactiveVal(NULL)
  results <- shiny::reactiveVal(NULL)

  # Accumulated district files. A single fileInput only keeps the most recent
  # selection, so we copy each upload to a stable per-session directory and
  # append it here. This lets the user add district files incrementally (upload
  # some, then add more) rather than picking them all in one dialog.
  #
  # Holds a data.frame with columns: uid, name, datapath. `uid` is a
  # session-unique counter used to key each file's name input. Keying by uid
  # (not by row position) means clearing the list and adding new files can
  # never inherit a stale name typed for a previous file that happened to sit
  # at the same position.
  districtFiles <- shiny::reactiveVal(NULL)
  nextUid <- shiny::reactiveVal(1L)

  # Geocode cache: geocoding is the slow, rate-limited step, so we run it once
  # per unique set of address/city values and reuse the points when only the
  # district files change. This is what lets you geocode once and then apply many
  # districting files. `geocodeKey` holds the address/city data the cache was
  # built from; when it changes, we re-geocode.
  geocodedData <- shiny::reactiveVal(NULL)
  geocodeKey <- shiny::reactiveVal(NULL)

  # Append each new upload to the accumulated list, copying to a persistent
  # directory so later uploads can't invalidate earlier temp files.
  shiny::observeEvent(input$districts, {
    shiny::req(input$districts)
    persist_dir <- file.path(tempdir(), "district_uploads")
    if (!dir.exists(persist_dir)) dir.create(persist_dir)

    n_new <- nrow(input$districts)
    stored <- vapply(seq_len(n_new), function(i) {
      # Preserve the ORIGINAL extension (shiny's temp name loses it), so the
      # read path can tell zips from direct .gpkg/.geojson uploads.
      ext <- tolower(tools::file_ext(input$districts$name[i]))
      dest <- tempfile("district_", tmpdir = persist_dir, fileext = paste0(".", ext))
      file.copy(input$districts$datapath[i], dest)
      dest
    }, character(1))

    uids <- nextUid() + seq_len(n_new) - 1L
    nextUid(nextUid() + n_new)

    new_files <- data.frame(
      uid = uids,
      name = input$districts$name,
      datapath = stored,
      stringsAsFactors = FALSE
    )
    districtFiles(rbind(districtFiles(), new_files))
  })

  # Clear the accumulated district files.
  shiny::observeEvent(input$clearDistricts, {
    districtFiles(NULL)
    shiny::showNotification("Cleared district files.", type = "message", duration = 3)
  })

  # Show a "Clear district files" button only once files have been added.
  output$clearDistrictsUI <- shiny::renderUI({
    shiny::req(districtFiles())
    shiny::actionButton("clearDistricts", "Clear district files")
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
      tryCatch({
        if (tolower(tools::file_ext(files$datapath[i])) == "zip") {
          unzip_and_read_spatial(files$datapath[i])
        } else {
          read_spatial_file(files$datapath[i])
        }
      }, error = function(e) {
        stop(paste0("Error reading district file '", files$name[i], "': ", e$message))
      })
    })
    districtNames <- vapply(seq_len(nrow(files)), function(i) {
      nm <- input[[paste0("districtName_", files$uid[i])]]
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
      shiny::showNotification("Reusing existing geocoding.", type = "message", duration = 2)
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
  output$geocodeUI <- shiny::renderUI({
    shiny::req(csvData(), input$streetColumn, input$cityColumn)
    shiny::actionButton("geocode", "Geocode Addresses")
  })

  # Geocode on demand, independent of district assignment.
  shiny::observeEvent(input$geocode, {
    shiny::req(input$memberList, input$streetColumn, input$cityColumn)
    tryCatch({
      memberList <- csvData()

      progress <- shiny::Progress$new()
      on.exit(progress$close())
      progress$set(message = "Preparing to geocode...", value = 0.1)

      # Only restrict to a district extent when asked AND files are available;
      # otherwise geocode unrestricted so this can run before districts exist.
      boundaries <- NULL
      if (isTRUE(input$restrictGeo)) {
        files <- districtFiles()
        if (is.null(files) || nrow(files) == 0) {
          shiny::showNotification(
            "Restrict is on but no district files added yet; geocoding without a restriction.",
            type = "warning", duration = 5
          )
        } else {
          boundaries <- compute_search_extent(read_prepared_layers())
        }
      }

      ensure_geocoded(memberList, input$streetColumn, input$cityColumn, boundaries, progress)
      progress$set(message = "Complete!", value = 1)
      shiny::showNotification("Geocoding complete.", type = "message", duration = 3)
    }, error = function(e) {
      shiny::showNotification(paste("Error:", e$message), type = "error", duration = NULL)
      print(paste("Geocode error:", e))
    })
  })

  # Live geocoding status shown under the Geocode button.
  output$geocodeStatus <- shiny::renderText({
    g <- geocodedData()
    if (is.null(g)) return("Addresses not geocoded yet.")
    n_ok      <- sum(g$geocode_status == "OK")
    n_fail    <- sum(g$geocode_status == "No geocode match")
    n_missing <- sum(g$geocode_status == "Missing address")
    sprintf("Geocoded %d of %d addresses (%d no match, %d missing address).",
            n_ok, nrow(g), n_fail, n_missing)
  })

  shiny::observeEvent(input$memberList, {
    shiny::req(input$memberList)
    tryCatch({
      # Shiny stores uploads under a temp name; read by the ORIGINAL file's
      # extension so .xlsx goes through readxl, not read.csv.
      ext <- tolower(tools::file_ext(input$memberList$name))
      path <- input$memberList$datapath
      if (ext %in% c("xlsx", "xls")) {
        renamed <- paste0(path, ".", ext)
        file.copy(path, renamed)
        path <- renamed
      }
      csvData(read_member_file(path))
      # A new member list invalidates any cached geocode.
      geocodedData(NULL)
      geocodeKey(NULL)
    }, error = function(e) {
      csvData(NULL)
      shiny::showNotification(paste("Could not read member list:", e$message),
                              type = "error", duration = NULL)
    })
  })

  output$columnSelection <- shiny::renderUI({
    shiny::req(csvData())
    columns <- colnames(csvData())

    # Set default selections
    default_street <- if ("Street.Address" %in% columns) "Street.Address" else columns[1]
    default_city <- if ("City" %in% columns) "City" else columns[1]

    shiny::tagList(
      shiny::selectInput("streetColumn", "Select Street Address Column",
                         choices = columns,
                         selected = default_street),
      shiny::selectInput("cityColumn", "Select City Column",
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
  # Inputs are keyed by each file's uid, so a cleared file's typed name can never
  # attach to a new file.
  output$districtNaming <- shiny::renderUI({
    shiny::req(districtFiles())
    files <- districtFiles()
    shiny::isolate({
      lapply(seq_len(nrow(files)), function(i) {
        input_id <- paste0("districtName_", files$uid[i])
        current <- input[[input_id]]
        default <- if (!is.null(current) && nzchar(current)) {
          current
        } else {
          tools::file_path_sans_ext(files$name[i])
        }
        shiny::textInput(
          inputId = input_id,
          label   = paste0("Name for district file (", files$name[i], ")"),
          value   = default
        )
      })
    })
  })

  # Manually clear the geocode cache so the next run geocodes from scratch.
  shiny::observeEvent(input$regeocode, {
    geocodedData(NULL)
    geocodeKey(NULL)
    shiny::showNotification("Geocode cache cleared. The next run will re-geocode.",
                            type = "message", duration = 4)
  })

  shiny::observeEvent(input$run, {
    shiny::req(input$memberList, input$streetColumn, input$cityColumn)

    tryCatch({
      memberList <- csvData()

      # Create a progress indicator
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      progress$set(message = "Loading district files...", value = 0.1)

      # Read, validate and name-prefix the accumulated district layers.
      layers <- read_prepared_layers()

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
      result <- AssignToDistricts(geocoded, layers, removeGEO = input$removeGEO)

      progress$set(message = "Complete!", value = 1)

      results(result)

      # Extract and display summary statistics
      stats <- attr(result, "summary_stats")
      if (!is.null(stats)) {
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

        shiny::showNotification(summary_msg, type = "message", duration = NULL)
      }
    }, error = function(e) {
      shiny::showNotification(paste("Error:", e$message), type = "error", duration = NULL)
      # Log the full error for debugging
      print(paste("Full error:", e))
    })
  })

  # Columns that can be shown in the table and written to CSV: atomic columns
  # only (drops any sf geometry list-column).
  exportable_columns <- function(df) {
    names(df)[vapply(df, is.atomic, logical(1))]
  }

  output$results <- DT::renderDT({
    shiny::req(results())
    result_data <- results()

    if (is.data.frame(result_data) && nrow(result_data) > 0) {
      result_data <- result_data[, exportable_columns(result_data), drop = FALSE]

      dt <- DT::datatable(result_data, options = list(scrollX = TRUE, pageLength = 5))

      # Highlight problem rows so they're visible instead of silently dropped:
      # anything not "OK" (missing address / no geocode match) gets a red tint.
      if ("geocode_status" %in% names(result_data)) {
        dt <- DT::formatStyle(
          dt, "geocode_status",
          target = "row",
          backgroundColor = DT::styleEqual("OK", "white", default = "#ffd9d9")
        )
      }
      # Flag low-confidence geocodes: a row can be "OK" yet matched with a low
      # score (wrong block, city-centroid fallback, ...). Amber-tint the score
      # cell below 85 so plausible-but-wrong matches get a second look.
      if ("geo_score" %in% names(result_data) && is.numeric(result_data$geo_score)) {
        dt <- DT::formatStyle(
          dt, "geo_score",
          backgroundColor = DT::styleInterval(85, c("#ffe9c6", "white"))
        )
      }
      dt
    } else {
      DT::datatable(data.frame(Message = "No valid results to display"))
    }
  })

  availableColumns <- shiny::reactiveVal(NULL)

  shiny::observeEvent(results(), {
    shiny::req(results())
    # Offer only columns that can actually be written to a CSV.
    availableColumns(exportable_columns(results()))
  })

  # Render UI for column selection
  output$columnSelectionForDownload <- shiny::renderUI({
    shiny::req(availableColumns())
    shiny::checkboxGroupInput("selectedColumns", "Select columns to download:",
                              choices = availableColumns(),
                              selected = NULL)
  })

  # Update download instructions
  output$downloadInstructions <- shiny::renderText({
    if (is.null(input$selectedColumns) || length(input$selectedColumns) == 0) {
      "Please select at least one column to download."
    } else {
      paste("Selected columns:", length(input$selectedColumns))
    }
  })

  output$downloadData <- shiny::downloadHandler(
    filename = function() {
      paste("assigned_districts_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      shiny::req(results(), input$selectedColumns)
      data_to_write <- results()

      selected_cols <- intersect(input$selectedColumns, names(data_to_write))
      if (length(selected_cols) > 0) {
        data_to_write <- data_to_write[, selected_cols, drop = FALSE]
        utils::write.csv(data_to_write, file, row.names = FALSE)
      } else {
        stop("Please select at least one column to download.")
      }
    }
  )
}
