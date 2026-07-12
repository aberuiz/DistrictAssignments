# District layer reading, validation, and naming.

# Turn an arbitrary label into a safe column-name prefix: non-alphanumeric runs
# collapse to a single underscore, and leading/trailing underscores are trimmed.
# e.g. "State Senate (2020)" -> "State_Senate_2020"
sanitize_layer_name <- function(name) {
  name <- gsub("[^A-Za-z0-9]+", "_", name)
  gsub("^_+|_+$", "", name)
}

# A layer's attribute (non-geometry) column names. The map dropdown's choices
# and the district-column heuristic's candidates both derive from this, so the
# "attribute columns = names minus the sf_column" convention lives in one place.
layer_attr_columns <- function(layer) {
  setdiff(names(layer), attr(layer, "sf_column"))
}

# Prefix a district layer's attribute (non-geometry) columns with its layer name
# so columns from multiple layers stay distinct and self-describing. A "DISTRICT"
# column in the "Congressional" layer becomes "Congressional_DISTRICT". The
# geometry column is left untouched. An empty/blank name is a no-op.
prefix_layer_columns <- function(layer, layer_name) {
  layer_name <- sanitize_layer_name(layer_name)
  if (nchar(layer_name) == 0) return(layer)

  data_cols <- layer_attr_columns(layer)
  if (length(data_cols) > 0) {
    idx <- match(data_cols, names(layer))
    names(layer)[idx] <- paste0(layer_name, "_", data_cols)
  }
  layer
}

# Best guess at the column that identifies a district within a prepared layer,
# used to color points and count members on the map. Columns are name-prefixed
# (e.g. "Congressional_DISTRICT"), so match against the BARE name (prefix
# stripped) and prefer district-like fields over incidental leading columns
# such as OBJECTID or Shape_Area -- picking the first column blindly would
# color a real TIGER file by its state FIPS code. Returns NA for a layer with
# no attribute columns (the map then falls back to a plain style).
layer_id_column <- function(layer, layer_name = NULL) {
  cols <- layer_attr_columns(layer)
  if (length(cols) == 0) return(NA_character_)
  bare <- cols
  if (!is.null(layer_name) && nzchar(layer_name)) {
    # The strip assumes sanitize_layer_name(list name) == the column prefix, an
    # invariant established by prepare_district_layers and locked by
    # test-layers.R's prefixed round-trip test.
    bare <- sub(paste0("^", sanitize_layer_name(layer_name), "_"), "", cols)
  }
  # "^cd[0-9]" matches TIGER congressional codes (CD118FP, CD116FP); the trailing
  # FP suffix means an anchored "^cd[0-9]*$" never would. It also matches oddballs
  # like CD2020POP, which is fine -- any CD...-prefixed column beats OBJECTID.
  patterns <- c("^district$", "^dist$", "namelsad", "^cd[0-9]", "^sldu",
                "^sldl", "ward", "precinct", "division", "^name$", "geoid", "name")
  for (p in patterns) {
    hit <- which(grepl(p, bare, ignore.case = TRUE))
    if (length(hit) > 0) return(cols[hit[1]])
  }
  cols[1]
}

# Determine a display name for each district layer, used to prefix its columns.
# Precedence, per layer:
#   1. an explicit districtNames value (if supplied and non-blank/non-NA)
#   2. the name attached to the districtsList element (named list)
#   3. the file's base name, when the element is still a file path
#   4. a positional fallback ("District_1", "District_2", ...)
resolve_layer_names <- function(districtsList, districtNames = NULL) {
  usable <- function(x) length(x) == 1 && !is.na(x) && nzchar(x)
  list_names <- names(districtsList)
  vapply(seq_along(districtsList), function(i) {
    if (!is.null(districtNames) && length(districtNames) >= i && usable(districtNames[i])) {
      return(districtNames[i])
    }
    if (!is.null(list_names) && usable(list_names[i])) {
      return(list_names[i])
    }
    el <- districtsList[[i]]
    if (is.character(el) && length(el) == 1) {
      return(tools::file_path_sans_ext(basename(el)))
    }
    paste0("District_", i)
  }, character(1))
}

# Validate and repair a layer's geometry on a planar (s2-off) basis, restoring
# the caller's s2 setting afterwards. Planar repair matches how the layers are
# later joined (see AssignToDistricts), so behavior doesn't depend on which file
# formats happened to be read first. st_make_valid() supersedes the old
# st_buffer(x, 0) repair trick, which is incorrect on longitude/latitude data.
repair_geometry <- function(layer) {
  old_s2 <- suppressMessages(sf::sf_use_s2(FALSE))
  on.exit(suppressMessages(sf::sf_use_s2(old_s2)))
  sf::st_make_valid(layer)
}

# Read a single .shp, .geojson, or .gpkg file from a path and return a
# validated, repaired sf layer.
read_spatial_file <- function(file_path) {
  file_ext <- tolower(tools::file_ext(file_path))

  if (length(file_ext) != 1) {
    stop("Invalid file extension")
  }

  switch(
    file_ext,
    "shp" = {
      # Let GDAL rebuild a missing .shx sidecar (GDAL reads config options from
      # the environment), restoring the previous value afterwards.
      old <- Sys.getenv("SHAPE_RESTORE_SHX", unset = NA)
      Sys.setenv(SHAPE_RESTORE_SHX = "YES")
      on.exit(
        if (is.na(old)) Sys.unsetenv("SHAPE_RESTORE_SHX")
        else Sys.setenv(SHAPE_RESTORE_SHX = old)
      )
      repair_geometry(sf::read_sf(file_path))
    },
    "geojson" = {
      repair_geometry(yyjsonr::read_geojson_file(file_path))
    },
    "gpkg" = {
      # A GeoPackage can hold several layers; use the first and say so, the
      # same "first match" convention used elsewhere in the package.
      layer_names <- sf::st_layers(file_path)$name
      if (length(layer_names) > 1) {
        message(sprintf(
          "GeoPackage '%s' contains %d layers (%s); using the first: '%s'.",
          basename(file_path), length(layer_names),
          paste(layer_names, collapse = ", "), layer_names[1]
        ))
      }
      repair_geometry(sf::read_sf(file_path, layer = layer_names[1]))
    },
    stop("Unsupported file format. Please provide a .shp, .geojson, or .gpkg file.")
  )
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

  spatial_files <- list.files(
    temp_dir,
    pattern = "\\.(shp|geojson|gpkg)$",
    full.names = TRUE, recursive = TRUE, ignore.case = TRUE
  )
  if (length(spatial_files) == 0) {
    stop("No .shp, .geojson, or .gpkg file found in the zip archive")
  }
  # Prefer formats in a stable order so archives holding several formats keep
  # reading the same file they always did (shapefile first).
  ext_rank <- match(tolower(tools::file_ext(spatial_files)), c("shp", "geojson", "gpkg"))
  read_spatial_file(spatial_files[order(ext_rank)][1])
}

#' Prepare district layers for assignment
#'
#' Normalize, read, validate and name-prefix one or more district layers so
#' they are ready to hand to [AssignToDistricts()].
#'
#' Each layer's attribute columns are prefixed with the layer's name (e.g. a
#' `DISTRICT` column in a layer named `Congressional` becomes
#' `Congressional_DISTRICT`) so columns from different layers stay distinct in
#' the joined output. Names are resolved per layer, in order of precedence:
#' an explicit `districtNames` entry, the name on the `districtsList` element,
#' the file's base name (for path elements), then a positional fallback.
#' Duplicate names are made unique (with a message), so any number of layers
#' -- including several files sharing a base name -- keep distinct, predictable
#' output columns. Layers without a CRS or without any features are rejected
#' with a clear error; a layer containing no polygons triggers a warning.
#'
#' @param districtsList A single sf layer or file path, a character vector of
#'   file paths, or an (optionally named) list of sf layers and/or file paths
#'   (`.shp` / `.geojson` / `.gpkg`). sf objects pass through; paths are read
#'   from disk.
#' @param districtNames Optional character vector of names, one per layer,
#'   used to prefix each layer's columns.
#'
#' @return A list of prepared sf layers with validated geometry, a coordinate
#'   reference system, and name-prefixed attribute columns.
#'
#' @examples
#' \dontrun{
#' layers <- prepare_district_layers(list(
#'   Congressional = "shapes/congress.shp",
#'   StateSenate   = "shapes/senate.geojson"
#' ))
#' }
#' @export
prepare_district_layers <- function(districtsList, districtNames = NULL) {
  # A character vector of paths becomes one layer per path.
  if (is.character(districtsList)) {
    districtsList <- as.list(districtsList)
  }
  # A single sf object is itself a list, so wrap anything that isn't already a
  # list-of-layers. inherits(x, "list") is TRUE only for real lists (not
  # data.frames/sf), so a lone layer is wrapped correctly.
  if (!inherits(districtsList, "list")) {
    districtsList <- list(districtsList)
  }

  # Resolve names BEFORE reading, so file-path basenames are still available to
  # fall back on (reading replaces paths with sf objects).
  layer_names <- resolve_layer_names(districtsList, districtNames)

  # Read any file-path elements into sf layers. read_spatial_file() already
  # repairs the geometry it reads, so track which elements were paths to avoid
  # repairing them a second time below.
  from_path <- vapply(districtsList,
                      function(x) is.character(x) && length(x) == 1, logical(1))
  for (i in seq_along(districtsList)) {
    if (from_path[i]) {
      districtsList[[i]] <- read_spatial_file(districtsList[[i]])
    }
  }

  for (i in seq_along(districtsList)) {
    # Every layer needs a CRS to be joined against geocoded (WGS84) points. A
    # shapefile missing its .prj sidecar is the common way to end up here.
    if (is.na(sf::st_crs(districtsList[[i]]))) {
      stop(sprintf(
        paste0("District layer '%s' has no coordinate reference system ",
               "(for a shapefile, the .prj sidecar file may be missing). ",
               "Set one with sf::st_set_crs() or re-export the layer with a CRS."),
        layer_names[i]
      ))
    }
    # An empty layer would assign NA everywhere and break the search extent;
    # it is always a wrong/corrupt upload, so fail loudly and early.
    if (nrow(districtsList[[i]]) == 0) {
      stop(sprintf("District layer '%s' contains no features.", layer_names[i]))
    }
    # Districts are polygons. Joining points against a point/line layer is
    # technically possible but would leave (nearly) every member unassigned,
    # so warn about what is almost certainly a wrong file.
    geom_types <- as.character(sf::st_geometry_type(districtsList[[i]]))
    if (!any(geom_types %in% c("POLYGON", "MULTIPOLYGON", "GEOMETRY", "GEOMETRYCOLLECTION"))) {
      warning(sprintf(
        "District layer '%s' contains no polygons (geometry: %s); member points are unlikely to fall within it.",
        layer_names[i], paste(unique(geom_types), collapse = ", ")
      ), call. = FALSE)
    }
  }

  # Column prefixes must be non-empty and unique across layers. With many
  # layers, duplicate names (two files both named "congress", or names that
  # sanitize to the same string) would produce colliding output columns that
  # sf silently renames to ".x"/".y", scrambling downstream lookups. Names
  # that sanitize to nothing fall back to their position.
  prefixes <- vapply(layer_names, sanitize_layer_name, character(1), USE.NAMES = FALSE)
  blank <- !nzchar(prefixes)
  prefixes[blank] <- paste0("District_", seq_along(prefixes)[blank])
  layer_names[blank] <- prefixes[blank]
  deduped <- make.unique(prefixes, sep = "_")
  changed <- deduped != prefixes
  if (any(changed)) {
    message(sprintf(
      "Duplicate district layer name(s) made unique: %s.",
      paste(sprintf("'%s' -> '%s'", layer_names[changed], deduped[changed]), collapse = ", ")
    ))
    layer_names[changed] <- deduped[changed]
    prefixes <- deduped
  }

  # Ensure valid geometry (only for directly-supplied sf layers; path inputs
  # were already repaired on read), then prefix each layer's columns with its
  # name so columns from different layers stay distinct in the joined output.
  districtsList[!from_path] <- lapply(districtsList[!from_path], repair_geometry)
  districtsList <- Map(prefix_layer_columns, districtsList, prefixes)

  # Name the list itself so downstream consumers (e.g. the app's map tab) can
  # refer to layers by display name.
  names(districtsList) <- layer_names
  districtsList
}

#' Compute a geocoding search extent from district layers
#'
#' Union bounding box of all layers, reprojected to WGS84 (EPSG:4326), suitable
#' for passing to the geocoder as a search extent. Using the union (rather than
#' just one layer) means the box covers the TOTAL area the districts span.
#'
#' Geocoding with a search extent is opt-in (see [AssignDistricts()]'s
#' `restrictToDistrictArea`). It is off by default because a search extent
#' hard-limits candidates to inside the box: a valid address just outside every
#' district would return no candidate and be mis-flagged as a geocode failure,
#' when really it's "found, just unassigned".
#'
#' @param layers A list of prepared sf layers (see [prepare_district_layers()]).
#' @param pad Fraction of the box's width/height added on every side (default
#'   5%). Keeps edge addresses from falsely failing to geocode.
#'
#' @return An [sf::st_bbox()] in EPSG:4326.
#' @export
compute_search_extent <- function(layers, pad = 0.05) {
  if (!inherits(layers, "list")) layers <- list(layers)
  if (length(layers) == 0) stop("At least one district layer is required to compute a search extent.")

  boxes <- lapply(layers, function(layer) sf::st_bbox(sf::st_transform(layer, 4326)))
  xmin <- min(vapply(boxes, function(b) b[["xmin"]], numeric(1)))
  ymin <- min(vapply(boxes, function(b) b[["ymin"]], numeric(1)))
  xmax <- max(vapply(boxes, function(b) b[["xmax"]], numeric(1)))
  ymax <- max(vapply(boxes, function(b) b[["ymax"]], numeric(1)))

  if (!all(is.finite(c(xmin, ymin, xmax, ymax)))) {
    stop("Could not compute a finite search extent from the district layers (does a layer have no features?).")
  }

  # Pad by a fraction of each span so edge addresses still geocode.
  dx <- (xmax - xmin) * pad
  dy <- (ymax - ymin) * pad

  sf::st_bbox(
    c(xmin = xmin - dx, ymin = ymin - dy, xmax = xmax + dx, ymax = ymax + dy),
    crs = sf::st_crs(4326)
  )
}
