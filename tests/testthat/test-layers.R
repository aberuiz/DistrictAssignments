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
