# Shiny app entry point. The app is a thin interface over the exported
# functions (prepare_district_layers, GeocodeMembers, AssignToDistricts), so
# the app and script workflows can't drift apart.

#' Launch the district-assignment Shiny app
#'
#' A point-and-click interface over [GeocodeMembers()] and
#' [AssignToDistricts()]: upload a member CSV, geocode it, add any number of
#' district files (`.gpkg`, `.geojson`, or zipped `.shp`), review the results
#' as a table or on a map, and download the assigned results. Requires the
#' suggested packages `shiny`, `DT`, and `bslib`; the map tab additionally
#' uses `leaflet` (a hint is shown if it isn't installed).
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

# The sidebar holds a lot of controls (upload, geocoding, district files,
# options, download), so it's grouped into accordion sections that stay
# compact even when many district files or columns are in play. Lists that
# grow with the user's data (per-file naming, download columns) are capped
# with their own scroll areas so the sidebar itself never becomes a
# pages-long scroll.
app_ui <- function() {
  bslib::page_sidebar(
    title = "Assign Districts",
    theme = bslib::bs_theme(version = 5, bootswatch = "minty"),
    sidebar = bslib::sidebar(
      width = 360,
      bslib::accordion(
        multiple = TRUE,
        open = c("Member data", "District files"),
        bslib::accordion_panel(
          "Member data",
          shiny::fileInput("memberList", "Upload Member List (CSV or Excel)",
                           accept = c(".csv", ".xlsx", ".xls")),
          # Choices are filled server-side (updateSelectizeInput with
          # server = TRUE) so member files with hundreds of columns don't
          # bloat the page.
          shiny::selectizeInput("streetColumn", "Street Address Column",
                                choices = NULL,
                                options = list(placeholder = "Upload a member list first")),
          shiny::selectizeInput("cityColumn", "City Column",
                                choices = NULL,
                                options = list(placeholder = "Upload a member list first")),
          shiny::uiOutput("geocodeUI"),
          shiny::textOutput("geocodeStatus")
        ),
        bslib::accordion_panel(
          "District files",
          shiny::fileInput("districts", "District Files (.gpkg, .geojson, or zipped .shp)",
                           accept = c(".zip", ".gpkg", ".geojson"),
                           multiple = TRUE,
                           buttonLabel = "Add File(s)..."),
          shiny::helpText(paste("You can keep adding district files. At least one is required.",
                                "Shapefiles must be zipped (with their sidecar files);",
                                "GeoPackage and GeoJSON can be uploaded directly or zipped.")),
          # Many uploads scroll inside this box instead of growing the sidebar.
          shiny::div(style = "max-height: 300px; overflow-y: auto;",
                     shiny::uiOutput("districtNaming")),
          shiny::uiOutput("clearDistrictsUI")
        ),
        bslib::accordion_panel(
          "Options",
          shiny::checkboxInput("removeGEO", "Remove Geometry Column", value = TRUE),
          shiny::checkboxInput("restrictGeo", "Restrict geocoding to district area", value = FALSE),
          shiny::checkboxInput("censusFallback",
                               "Retry failed geocodes with the US Census geocoder", value = TRUE),
          shiny::actionButton("regeocode", "Clear geocode cache",
                              class = "btn-outline-secondary btn-sm"),
          shiny::helpText("Clear the cache to force a fresh geocode.")
        )
      ),
      shiny::actionButton("run", "Assign Districts", class = "btn-primary w-100"),
      # Closed until results exist; the server pops it open after a run.
      bslib::accordion(
        id = "downloadAccordion",
        open = FALSE,
        bslib::accordion_panel(
          "Download",
          shiny::textOutput("downloadInstructions"),
          shiny::uiOutput("columnSelectionForDownload"),
          shiny::downloadButton("downloadData", "Download Selected Columns",
                                class = "w-100")
        )
      )
    ),
    bslib::navset_card_underline(
      bslib::nav_panel("Table", DT::DTOutput("results")),
      # Read-only results map (requires the suggested leaflet package):
      # polygons per layer as toggleable groups, points colored by the
      # selected layer's assignment, per-district member counts.
      bslib::nav_panel("Map", shiny::uiOutput("mapUI"))
    )
  )
}

app_server <- function(input, output, session) {

  csvData <- shiny::reactiveVal(NULL)
  results <- shiny::reactiveVal(NULL)

  # The prepared (named) layers the current results were assigned against;
  # set together with results() so the map can never show polygons from a
  # different run than the points.
  assignedLayers <- shiny::reactiveVal(NULL)

  # Best guess at the column that identifies a district within a prepared layer,
  # used to color points and count members on the map. Columns are name-prefixed
  # (e.g. "Congressional_DISTRICT"), so match against the BARE name (prefix
  # stripped) and prefer district-like fields over incidental leading columns
  # such as OBJECTID or Shape_Area -- picking the first column blindly would
  # color a real TIGER file by its state FIPS code. Returns NA for a layer with
  # no attribute columns (the map then falls back to a plain style).
  layer_id_column <- function(layer, layer_name = NULL) {
    cols <- setdiff(names(layer), attr(layer, "sf_column"))
    if (length(cols) == 0) return(NA_character_)
    bare <- cols
    if (!is.null(layer_name) && nzchar(layer_name)) {
      bare <- sub(paste0("^", sanitize_layer_name(layer_name), "_"), "", cols)
    }
    patterns <- c("^district$", "^dist$", "namelsad", "^cd[0-9]*$", "^sldu",
                  "^sldl", "ward", "precinct", "division", "^name$", "geoid", "name")
    for (p in patterns) {
      hit <- which(grepl(p, bare, ignore.case = TRUE))
      if (length(hit) > 0) return(cols[hit[1]])
    }
    cols[1]
  }

  # Resolve the district-id column for the currently selected map layer: the
  # user's explicit "using column" pick when it is valid for that layer,
  # otherwise the heuristic default.
  selected_id_column <- function(layers, layer_name, chosen) {
    layer <- layers[[layer_name]]
    if (!is.null(chosen) && length(chosen) == 1 && nzchar(chosen) &&
        chosen %in% names(layer)) {
      return(chosen)
    }
    layer_id_column(layer, layer_name)
  }

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
    files <- districtFiles()
    if (!is.null(files)) unlink(files$datapath)
    districtFiles(NULL)
    shiny::showNotification("Cleared district files.", type = "message", duration = 3)
  })

  # Show a "Clear district files" button only once files have been added.
  output$clearDistrictsUI <- shiny::renderUI({
    shiny::req(districtFiles())
    shiny::actionButton("clearDistricts", "Clear district files",
                        class = "btn-outline-danger btn-sm")
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
      bbox   = bbox_signature(boundaries),
      census = isTRUE(input$censusFallback)
    )
    if (!is.null(geocodedData()) && identical(key, geocodeKey())) {
      shiny::showNotification("Reusing existing geocoding.", type = "message", duration = 2)
      return(geocodedData())
    }
    if (!is.null(progress)) progress$set(message = "Geocoding addresses...", value = 0.3)
    geocoded <- GeocodeMembers(memberList, streetCol, cityCol, boundaries = boundaries,
                               censusFallback = isTRUE(input$censusFallback))
    geocodedData(geocoded)
    geocodeKey(key)
    # A chunk of requests can fail without sinking the run (see
    # GeocodeMembers); warnings don't surface in Shiny, so tell the user here
    # -- including how to retry, since the failed result is now cached.
    n_err <- sum(geocoded$geocode_status == "Geocoder error")
    if (n_err > 0) {
      shiny::showNotification(
        sprintf(paste("%d address(es) hit a geocoder service error and were not tried.",
                      "Press 'Clear geocode cache' and geocode again to retry them."), n_err),
        type = "warning", duration = NULL
      )
    }
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
    n_err     <- sum(g$geocode_status == "Geocoder error")
    n_census  <- if (is.null(g$geo_source)) 0 else sum(g$geo_source == "Census", na.rm = TRUE)
    msg <- sprintf("Geocoded %d of %d addresses (%d no match, %d missing address).",
                   n_ok, nrow(g), n_fail, n_missing)
    if (n_err > 0) {
      msg <- paste0(msg, sprintf(" %d hit a geocoder error (clear the cache to retry).", n_err))
    }
    if (n_census > 0) {
      msg <- paste0(msg, sprintf(" %d recovered by the Census fallback.", n_census))
    }
    msg
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

      # Fill the (static, server-side) column selectors. server = TRUE keeps
      # the choice list on the server, so files with hundreds of columns stay
      # fast and searchable.
      columns <- colnames(csvData())
      shiny::updateSelectizeInput(
        session, "streetColumn", choices = columns,
        selected = if ("Street.Address" %in% columns) "Street.Address" else columns[1],
        server = TRUE
      )
      shiny::updateSelectizeInput(
        session, "cityColumn", choices = columns,
        selected = if ("City" %in% columns) "City" else columns[1],
        server = TRUE
      )
    }, error = function(e) {
      csvData(NULL)
      shiny::updateSelectizeInput(session, "streetColumn", choices = character(0), server = TRUE)
      shiny::updateSelectizeInput(session, "cityColumn", choices = character(0), server = TRUE)
      shiny::showNotification(paste("Could not read member list:", e$message),
                              type = "error", duration = NULL)
    })
  })

  # One card per accumulated district file: the file name (truncated with an
  # ellipsis, full name in the tooltip, so long names can't stretch the
  # sidebar), a remove button, and a text input naming the layer. The name
  # becomes the prefix on that layer's output columns (e.g. a name of
  # "Congressional" turns a "DISTRICT" column into "Congressional_DISTRICT").
  # Defaults to the uploaded file's base name.
  #
  # This UI re-renders whenever a file is added or removed, which recreates the
  # text boxes. We isolate() the current input values and reuse them as defaults
  # so any names the user already typed are preserved instead of reverting to
  # the file name. Inputs are keyed by each file's uid, so a removed file's
  # typed name can never attach to a new file.
  output$districtNaming <- shiny::renderUI({
    shiny::req(districtFiles())
    files <- districtFiles()
    shiny::isolate({
      cards <- lapply(seq_len(nrow(files)), function(i) {
        input_id <- paste0("districtName_", files$uid[i])
        current <- input[[input_id]]
        default <- if (!is.null(current) && nzchar(current)) {
          current
        } else {
          tools::file_path_sans_ext(files$name[i])
        }
        shiny::div(
          class = "border rounded p-2 mb-2",
          shiny::div(
            class = "d-flex justify-content-between align-items-center mb-1",
            shiny::tags$small(
              class = "text-muted text-truncate me-2",
              style = "min-width: 0;",  # lets text-truncate work inside flex
              title = files$name[i],
              files$name[i]
            ),
            shiny::tags$button(
              type = "button", class = "btn-close flex-shrink-0",
              title = "Remove this file",
              onclick = sprintf(
                "Shiny.setInputValue('removeDistrictUid', %d, {priority: 'event'})",
                files$uid[i]
              )
            )
          ),
          shiny::textInput(input_id, label = NULL, value = default, width = "100%")
        )
      })
      shiny::tagList(
        shiny::helpText("Each name becomes the prefix on that layer's output columns."),
        cards
      )
    })
  })

  # Remove a single district file (the X on its card).
  shiny::observeEvent(input$removeDistrictUid, {
    files <- districtFiles()
    shiny::req(files)
    drop <- files$uid == input$removeDistrictUid
    if (any(drop)) {
      unlink(files$datapath[drop])
      remaining <- files[!drop, , drop = FALSE]
      districtFiles(if (nrow(remaining) > 0) remaining else NULL)
    }
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
      assignedLayers(layers)

      # Show the summary statistics in a modal: a notification renders its
      # text as collapsed HTML (newlines vanish) and is too small for a
      # multi-line report anyway.
      stats <- attr(result, "summary_stats")
      if (!is.null(stats)) {
        summary_list <- shiny::tags$ul(
          shiny::tags$li(sprintf("Total addresses: %d", stats$original_count)),
          shiny::tags$li(sprintf("Invalid addresses (blank/NA): %d",
                                 stats$invalid_address_count)),
          shiny::tags$li(sprintf("Failed geocoding: %d", stats$failed_geocoding_count)),
          shiny::tags$li(sprintf("Successfully geocoded: %d", stats$successful_count)),
          if (isTRUE(stats$geocoder_error_count > 0)) {
            shiny::tags$li(sprintf("Geocoder request errors (retryable): %d",
                                   stats$geocoder_error_count))
          }
        )

        # Details about failed rows, if any; cap the list so a large upload
        # with many bad addresses stays readable (the table's geocode_status
        # column has them all), and scroll what remains.
        failed_block <- NULL
        if (stats$failed_geocoding_count > 0 && !is.null(stats$failed_geocoding_rows)) {
          fr <- utils::head(stats$failed_geocoding_rows, 10)
          failed_lines <- sprintf("Row %d: %s, %s",
                                  fr$original_row_id, fr$Street.Address, fr$City)
          if (stats$failed_geocoding_count > nrow(fr)) {
            failed_lines <- c(failed_lines,
                              sprintf("... and %d more (see the geocode_status column)",
                                      stats$failed_geocoding_count - nrow(fr)))
          }
          failed_block <- shiny::tagList(
            shiny::tags$strong("Failed addresses:"),
            shiny::tags$div(
              style = "white-space: pre-wrap; max-height: 200px; overflow-y: auto;",
              paste(failed_lines, collapse = "\n")
            )
          )
        }

        shiny::showModal(shiny::modalDialog(
          title = "Processing complete",
          summary_list,
          failed_block,
          easyClose = TRUE,
          footer = shiny::modalButton("Close")
        ))
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
    # When removeGEO = FALSE the result is an sf object; its geometry column is
    # "sticky" and survives `[` column selection, so drop it first or it would
    # render (and later export) as a stringified list-column.
    if (inherits(result_data, "sf")) result_data <- sf::st_drop_geometry(result_data)

    if (is.data.frame(result_data) && nrow(result_data) > 0) {
      result_data <- result_data[, exportable_columns(result_data), drop = FALSE]

      # Built for wide results: a "Show/hide columns" button (colvis) tames
      # many district layers' worth of columns, per-column filters help find
      # rows in long lists, and long cell text is truncated at 40 characters
      # with the full value in the hover tooltip.
      dt <- DT::datatable(
        result_data,
        filter = "top",
        extensions = "Buttons",
        plugins = "ellipsis",
        options = list(
          scrollX = TRUE,
          pageLength = 25,
          dom = "Blfrtip",
          buttons = list(list(extend = "colvis", text = "Show/hide columns")),
          columnDefs = list(list(
            targets = "_all",
            render = DT::JS("$.fn.dataTable.render.ellipsis(40, false)")
          ))
        )
      )

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

  # ---- Results map ---------------------------------------------------------

  output$mapUI <- shiny::renderUI({
    if (!requireNamespace("leaflet", quietly = TRUE)) {
      return(shiny::helpText(
        'Install the "leaflet" package to see the results map: install.packages("leaflet")'
      ))
    }
    if (is.null(results()) || is.null(assignedLayers())) {
      return(shiny::helpText("Run Assign Districts to see the results on a map."))
    }
    shiny::tagList(
      shiny::selectInput("mapLayer", "Color points by district layer:",
                         choices = names(assignedLayers())),
      # Which attribute column of the selected layer identifies a district. The
      # choices (and a heuristic default) are filled server-side whenever the
      # selected layer changes, so the user can override a bad guess -- e.g. a
      # file whose first column is OBJECTID rather than the district name.
      shiny::selectInput("mapIdCol", "using column:", choices = NULL),
      # Sized against the viewport (not a fixed pixel height) and marked
      # flex-shrink: 0 so the fillable card can't squeeze the map to make room
      # for the counts table below -- the panel scrolls to the table instead.
      shiny::div(
        style = "height: 50vh; min-height: 320px; flex-shrink: 0;",
        leaflet::leafletOutput("map", height = "100%")
      ),
      shiny::textOutput("mapCaption"),
      DT::DTOutput("mapCounts")
    )
  })

  # Fill the "using column" picker for whichever layer is selected, defaulting
  # to the heuristic guess. Fires on the initial layer selection too, so the
  # picker is populated before the map first draws.
  shiny::observeEvent(input$mapLayer, {
    layers <- assignedLayers()
    shiny::req(layers, input$mapLayer %in% names(layers))
    layer <- layers[[input$mapLayer]]
    cols <- setdiff(names(layer), attr(layer, "sf_column"))
    default <- layer_id_column(layer, input$mapLayer)
    shiny::updateSelectInput(session, "mapIdCol", choices = cols,
                             selected = if (!is.na(default)) default else character(0))
  })

  # Registered only when leaflet is installed; without it the map tab shows
  # the install hint from mapUI instead.
  if (requireNamespace("leaflet", quietly = TRUE)) output$map <- leaflet::renderLeaflet({
    shiny::req(results(), assignedLayers(), input$mapLayer)
    layers <- assignedLayers()
    shiny::req(input$mapLayer %in% names(layers))
    res <- results()
    if (inherits(res, "sf")) res <- sf::st_drop_geometry(res)

    # District-id column for the selected layer: the user's "using column" pick
    # when valid, else the heuristic. Used for point coloring, its polygon
    # labels, and the counts table (which resolves it the same way).
    sel_col <- selected_id_column(layers, input$mapLayer, input$mapIdCol)

    m <- leaflet::leaflet() |> leaflet::addTiles()

    # All layers' polygons, one toggleable group per layer, labeled with the
    # per-district member count (computed from the same results the table
    # shows, so the numbers can never disagree). The selected layer follows the
    # "using column" pick; the rest use their heuristic id column.
    for (nm in names(layers)) {
      l4326 <- sf::st_transform(layers[[nm]], 4326)
      col <- if (identical(nm, input$mapLayer)) sel_col else layer_id_column(l4326, nm)
      label <- if (!is.na(col) && col %in% names(res)) {
        counts <- table(res[[col]])
        v <- as.character(l4326[[col]])
        n <- as.integer(counts[v])
        n[is.na(n)] <- 0L
        sprintf("%s: %s \u2014 %d member%s", nm, v, n, ifelse(n == 1, "", "s"))
      } else {
        rep(nm, nrow(l4326))
      }
      m <- leaflet::addPolygons(m, data = l4326, group = nm,
                                weight = 1.5, fillOpacity = 0.15, label = label)
    }

    # Points, colored by the selected layer's assignment; geocoded rows that
    # fall outside every district plot gray.
    pts <- res[!is.na(res$geo_x) & !is.na(res$geo_y), , drop = FALSE]
    if (nrow(pts) > 0) {
      addr <- htmltools::htmlEscape(paste0(pts$Street.Address, ", ", pts$City))
      if (!is.na(sel_col) && sel_col %in% names(pts)) {
        vals <- as.character(pts[[sel_col]])
        pal <- leaflet::colorFactor("viridis", domain = sort(unique(stats::na.omit(vals))))
        point_color <- ifelse(is.na(vals), "#888888", pal(vals))
        popup <- paste0(addr, "<br>", htmltools::htmlEscape(input$mapLayer), ": ",
                        htmltools::htmlEscape(ifelse(is.na(vals), "unassigned", vals)))
      } else {
        point_color <- "#3388ff"
        popup <- addr
      }
      m <- leaflet::addCircleMarkers(m, lng = pts$geo_x, lat = pts$geo_y,
                                     radius = 5, weight = 1, fillOpacity = 0.85,
                                     color = point_color, popup = popup)
    }

    # Keep the control expanded while it's small; with many layers it would
    # cover a big corner of the map, so collapse it to the icon instead.
    m <- leaflet::addLayersControl(
      m, overlayGroups = names(layers),
      options = leaflet::layersControlOptions(collapsed = length(layers) > 5)
    )
    # Start with only the selected layer's polygons visible; the control lets
    # the user toggle the rest on.
    for (nm in setdiff(names(layers), input$mapLayer)) {
      m <- leaflet::hideGroup(m, nm)
    }
    m
  })

  output$mapCaption <- shiny::renderText({
    shiny::req(results())
    res <- results()
    n_missing <- sum(is.na(res$geo_x) | is.na(res$geo_y))
    sprintf("%d of %d rows plotted. %d row%s without coordinates not shown.",
            nrow(res) - n_missing, nrow(res),
            n_missing, if (n_missing == 1) "" else "s")
  })

  # Rendered with DT (paged + searchable) rather than a plain table: a layer
  # with hundreds of districts (precincts, wards) would otherwise append an
  # arbitrarily tall table below the map.
  output$mapCounts <- DT::renderDT({
    shiny::req(results(), assignedLayers(), input$mapLayer)
    layers <- assignedLayers()
    shiny::req(input$mapLayer %in% names(layers))
    # Same column the map colors by (user pick or heuristic); input$mapIdCol is
    # referenced so the table refreshes when the "using column" pick changes.
    col <- selected_id_column(layers, input$mapLayer, input$mapIdCol)
    res <- results()
    shiny::req(!is.na(col), col %in% names(res))

    counts <- table(res[[col]], useNA = "no")
    df <- data.frame(District = names(counts), Members = as.integer(counts))
    n_unassigned <- sum(!is.na(res$geo_x) & !is.na(res$geo_y) & is.na(res[[col]]))
    if (n_unassigned > 0) {
      df <- rbind(df, data.frame(District = "(geocoded, outside all districts)",
                                 Members = n_unassigned))
    }
    DT::datatable(df, rownames = FALSE,
                  options = list(pageLength = 10, dom = "ftip"))
  })

  # ---- Download ------------------------------------------------------------

  availableColumns <- shiny::reactiveVal(NULL)

  shiny::observeEvent(results(), {
    shiny::req(results())
    # Offer only columns that can actually be written to a CSV.
    availableColumns(exportable_columns(results()))
    # Results exist, so downloading is now meaningful: open the panel.
    bslib::accordion_panel_open("downloadAccordion", "Download")
  })

  # Checked by default: the user's original columns plus the district
  # assignments. The geocoder bookkeeping columns (original_row_id,
  # geocode_status, geo_*) are offered but unchecked -- most users don't want
  # them once districting is done.
  default_download_columns <- function(cols) {
    cols[!(cols %in% c("original_row_id", "geocode_status") | startsWith(cols, "geo_"))]
  }

  # Column picker for the download. The checkbox list scrolls in its own box
  # so many district layers' worth of columns can't stretch the sidebar.
  output$columnSelectionForDownload <- shiny::renderUI({
    shiny::req(availableColumns())
    shiny::tagList(
      shiny::div(
        class = "mb-1",
        shiny::actionLink("selectAllColumns", "Select all"),
        " | ",
        shiny::actionLink("selectNoColumns", "Select none")
      ),
      shiny::div(
        class = "border rounded p-2",
        style = "max-height: 250px; overflow-y: auto;",
        shiny::checkboxGroupInput("selectedColumns", NULL,
                                  choices = availableColumns(),
                                  selected = default_download_columns(availableColumns()))
      )
    )
  })

  shiny::observeEvent(input$selectAllColumns, {
    shiny::updateCheckboxGroupInput(session, "selectedColumns",
                                    selected = availableColumns())
  })
  shiny::observeEvent(input$selectNoColumns, {
    shiny::updateCheckboxGroupInput(session, "selectedColumns",
                                    selected = character(0))
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
      # Drop the sticky sf geometry (removeGEO = FALSE) before selecting columns;
      # otherwise write.csv emits a bogus "geometry" column of stringified points.
      if (inherits(data_to_write, "sf")) data_to_write <- sf::st_drop_geometry(data_to_write)

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
