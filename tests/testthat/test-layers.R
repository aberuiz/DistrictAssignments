test_that("sanitize_layer_name collapses punctuation and trims underscores", {
  expect_equal(sanitize_layer_name("State Senate (2020)"), "State_Senate_2020")
  expect_equal(sanitize_layer_name("__already__ok__"), "already_ok")
  expect_equal(sanitize_layer_name("!!!"), "")
})

test_that("prefix_layer_columns prefixes attributes but not geometry", {
  layer <- fixture_layer_ns()
  out <- prefix_layer_columns(layer, "Congressional")
  expect_true("Congressional_DISTRICT" %in% names(out))
  expect_equal(attr(out, "sf_column"), attr(layer, "sf_column"))

  # blank name is a no-op
  expect_equal(names(prefix_layer_columns(layer, "")), names(layer))
})

test_that("resolve_layer_names follows the documented precedence", {
  lst <- list(A = "path/to/congress.shp", "path/to/senate.geojson", fixture_layer_ns())
  # explicit names win; NA and "" fall through to the next source
  expect_equal(
    resolve_layer_names(lst, c("Explicit", NA, "")),
    c("Explicit", "senate", "District_3")
  )
  # list names next, then file basename, then positional fallback
  expect_equal(resolve_layer_names(lst), c("A", "senate", "District_3"))
})

test_that("prepare_district_layers accepts sf, named lists, and character vectors", {
  layer <- fixture_layer_ns()

  # lone sf object
  out <- prepare_district_layers(layer, "Council")
  expect_length(out, 1)
  expect_true("Council_DISTRICT" %in% names(out[[1]]))

  # named list of sf objects
  out <- prepare_district_layers(list(Council = layer, Zones = fixture_layer_overlap()))
  expect_true("Council_DISTRICT" %in% names(out[[1]]))
  expect_true("Zones_ZONE" %in% names(out[[2]]))

  # character vector of paths (previously errored)
  path <- tempfile(fileext = ".geojson")
  sf::st_write(layer, path, quiet = TRUE)
  out <- prepare_district_layers(c(path, path))
  expect_length(out, 2)
  prefix <- tools::file_path_sans_ext(basename(path))
  expect_true(paste0(prefix, "_DISTRICT") %in% names(out[[1]]))
})

test_that("prepare_district_layers accepts a mixed paths + sf list", {
  # A list holding both a file path and an in-memory sf layer exercises the
  # read/prefix split for both element kinds in one call.
  path <- tempfile(fileext = ".geojson")
  sf::st_write(fixture_layer_ns(), path, quiet = TRUE)

  out <- prepare_district_layers(list(FromPath = path, InMem = fixture_layer_overlap()))
  expect_length(out, 2)
  expect_true("FromPath_DISTRICT" %in% names(out[[1]]))
  expect_true("InMem_ZONE" %in% names(out[[2]]))
})

test_that("prepare_district_layers repairs geometry on both input paths", {
  # Passed a bowtie sf layer directly, geometry must come back valid.
  out <- prepare_district_layers(list(Bad = fixture_layer_bowtie()))
  expect_true(all(sf::st_is_valid(out[[1]])))

  # Same layer written to disk and passed as a path: reads no longer repair, so
  # this proves prepare_district_layers is the single repair point.
  path <- tempfile(fileext = ".geojson")
  sf::st_write(fixture_layer_bowtie(), path, quiet = TRUE)
  out <- prepare_district_layers(path)
  expect_true(all(sf::st_is_valid(out[[1]])))
})

test_that("prepare_district_layers returns a list named by layer", {
  out <- prepare_district_layers(
    list(fixture_layer_ns(), fixture_layer_overlap()),
    districtNames = c("Council", "Zones")
  )
  expect_equal(names(out), c("Council", "Zones"))
})

test_that("prepare_district_layers rejects a layer with no CRS", {
  no_crs <- sf::st_sf(DISTRICT = "D1", geometry = sf::st_sfc(mk_poly(-98, 30, -97.5, 30.6)))
  expect_error(
    prepare_district_layers(list(Bad = no_crs)),
    "no coordinate reference system"
  )
})

test_that("GeoPackage files are read directly and inside zips", {
  layer <- fixture_layer_ns()
  path <- tempfile(fileext = ".gpkg")
  sf::st_write(layer, path, quiet = TRUE)

  out <- read_spatial_file(path)
  expect_s3_class(out, "sf")
  expect_equal(nrow(out), 2)

  # multi-layer GeoPackage: first layer is used, with a message naming it
  multi <- tempfile(fileext = ".gpkg")
  sf::st_write(layer, multi, layer = "first", quiet = TRUE)
  sf::st_write(fixture_layer_overlap(), multi, layer = "second", quiet = TRUE)
  expect_message(out <- read_spatial_file(multi), "using the first")
  expect_true("DISTRICT" %in% names(out))

  # zipped GeoPackage goes through unzip_and_read_spatial
  dir <- tempfile("gpkgzip_")
  dir.create(dir)
  file.copy(path, file.path(dir, "districts.gpkg"))
  zip_path <- tempfile(fileext = ".zip")
  old_wd <- setwd(dir)
  on.exit(setwd(old_wd))
  utils::zip(zip_path, "districts.gpkg", flags = "-q")
  setwd(old_wd)
  out <- unzip_and_read_spatial(zip_path)
  expect_equal(nrow(out), 2)
})

test_that("read_spatial_file rejects unsupported formats", {
  path <- tempfile(fileext = ".txt")
  writeLines("not spatial", path)
  expect_error(read_spatial_file(path), "Unsupported file format")
})

test_that("unzip_and_read_spatial reads a zipped geojson and isolates archives", {
  layer <- fixture_layer_ns()
  dir <- tempfile("zipsrc_")
  dir.create(dir)
  geo_path <- file.path(dir, "districts.geojson")
  sf::st_write(layer, geo_path, quiet = TRUE)

  zip_path <- tempfile(fileext = ".zip")
  old_wd <- setwd(dir)
  on.exit(setwd(old_wd))
  utils::zip(zip_path, "districts.geojson", flags = "-q")
  setwd(old_wd)

  out <- unzip_and_read_spatial(zip_path)
  expect_s3_class(out, "sf")
  expect_equal(nrow(out), 2)

  # an archive without spatial files errors clearly
  empty_zip <- tempfile(fileext = ".zip")
  txt <- file.path(dir, "readme.txt")
  writeLines("x", txt)
  setwd(dir)
  utils::zip(empty_zip, "readme.txt", flags = "-q")
  setwd(old_wd)
  expect_error(unzip_and_read_spatial(empty_zip), "No .shp, .geojson, or .gpkg")
})

test_that("compute_search_extent unions and pads layer boxes", {
  b <- compute_search_extent(list(fixture_layer_ns(), fixture_layer_overlap()), pad = 0)
  expect_equal(as.numeric(b), c(-99, 29, -97, 31))

  padded <- compute_search_extent(list(fixture_layer_ns()), pad = 0.05)
  expect_lt(padded[["xmin"]], -98)
  expect_gt(padded[["xmax"]], -97.5)

  expect_error(compute_search_extent(list()), "At least one district layer")
})

test_that("duplicate layer names are made unique with distinct prefixes", {
  expect_message(
    out <- prepare_district_layers(
      list(Council = fixture_layer_ns(), Council = fixture_layer_overlap())
    ),
    "made unique"
  )
  expect_equal(names(out), c("Council", "Council_1"))
  expect_true("Council_DISTRICT" %in% names(out[[1]]))
  expect_true("Council_1_ZONE" %in% names(out[[2]]))
})

test_that("names that sanitize to nothing fall back to a positional prefix", {
  out <- suppressMessages(
    prepare_district_layers(list(fixture_layer_ns()), districtNames = "!!!")
  )
  expect_equal(names(out), "District_1")
  expect_true("District_1_DISTRICT" %in% names(out[[1]]))
})

test_that("layer_id_column recognizes trimmed TIGER congressional codes", {
  # A CD shapefile stripped to a few columns: no NAMELSAD/GEOID/NAME to catch,
  # so the CD code must win over the leading OBJECTID (the F3 regression).
  cd <- sf::st_sf(
    OBJECTID = 1:2, CD118FP = c("01", "02"), ALAND = c(1, 2), AWATER = c(0, 0),
    geometry = sf::st_sfc(mk_poly(-98, 30, -97.5, 30.3),
                          mk_poly(-98, 30.3, -97.5, 30.6), crs = 4326)
  )
  expect_equal(layer_id_column(cd), "CD118FP")
})

test_that("layer_id_column follows its priority order and edge cases", {
  # NAMELSAD outranks the CD code in a full TIGER-ish layer.
  full <- sf::st_sf(
    STATEFP = "48", CD118FP = "01", GEOID = "4801", NAMELSAD = "District 1",
    geometry = sf::st_sfc(mk_poly(-98, 30, -97.5, 30.6), crs = 4326)
  )
  expect_equal(layer_id_column(full), "NAMELSAD")

  # A ward column is picked out of unrelated leading columns.
  ward <- sf::st_sf(
    OBJECTID = 1, WARDID = "3",
    geometry = sf::st_sfc(mk_poly(-98, 30, -97.5, 30.6), crs = 4326)
  )
  expect_equal(layer_id_column(ward), "WARDID")

  # No attribute columns -> NA (map falls back to a plain style).
  geom_only <- sf::st_sf(
    geometry = sf::st_sfc(mk_poly(-98, 30, -97.5, 30.6), crs = 4326)
  )
  expect_true(is.na(layer_id_column(geom_only)))

  # Nothing recognized -> the first attribute column.
  unknown <- sf::st_sf(
    FOO = 1, BAR = 2,
    geometry = sf::st_sfc(mk_poly(-98, 30, -97.5, 30.6), crs = 4326)
  )
  expect_equal(layer_id_column(unknown), "FOO")
})

test_that("layer_id_column strips the layer-name prefix that prepare_district_layers adds", {
  # Two layers sharing a name force the make.unique dedupe branch, so the second
  # layer's prefix is "Council_1" rather than "Council". For each element, the
  # heuristic must still find a prefixed column that exists in the layer --
  # proving sanitize_layer_name(list name) still equals the real column prefix.
  out <- suppressMessages(prepare_district_layers(
    list(Council = fixture_layer_ns(), Council = fixture_layer_overlap())
  ))
  for (nm in names(out)) {
    col <- layer_id_column(out[[nm]], nm)
    expect_true(startsWith(col, paste0(nm, "_")))
    expect_true(col %in% names(out[[nm]]))
  }
})

test_that("empty and non-polygon layers are caught", {
  expect_error(
    prepare_district_layers(list(Empty = fixture_layer_ns()[0, ])),
    "contains no features"
  )

  pt_layer <- sf::st_sf(
    ID = 1,
    geometry = sf::st_sfc(sf::st_point(c(-97.7, 30.2)), crs = 4326)
  )
  expect_warning(
    prepare_district_layers(list(Points = pt_layer)),
    "contains no polygons"
  )

  # an empty layer also can't silently produce an infinite search extent
  expect_error(
    compute_search_extent(list(fixture_layer_ns()[0, ])),
    "finite search extent"
  )
})
