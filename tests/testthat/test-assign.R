test_that("AssignToDistricts keeps every row aligned and fills NA for unlocated", {
  layers <- prepare_district_layers(list(Council = fixture_layer_ns()))
  res <- AssignToDistricts(fixture_geocoded(), layers, verbose = FALSE)

  expect_equal(nrow(res), 4)
  expect_equal(res$original_row_id, 1:4)
  expect_equal(res$Council_DISTRICT, c("South", "North", NA, NA))
  expect_false("geometry" %in% names(res))
})

test_that("AssignToDistricts joins multiple layers with prefixed columns", {
  layers <- prepare_district_layers(list(
    Council = fixture_layer_ns(),
    Zones   = fixture_layer_overlap()
  ))
  res <- suppressMessages(AssignToDistricts(fixture_geocoded(), layers, verbose = FALSE))

  expect_equal(res$Council_DISTRICT, c("South", "North", NA, NA))
  expect_equal(res$Zones_ZONE, c("Z1", "Z1", NA, NA))
})

test_that("overlapping polygons keep first match and message once", {
  layers <- prepare_district_layers(list(Zones = fixture_layer_overlap()))
  expect_message(
    res <- AssignToDistricts(fixture_geocoded(), layers),
    "fell within multiple polygons"
  )
  expect_equal(nrow(res), 4)
  expect_equal(res$Zones_ZONE, c("Z1", "Z1", NA, NA))
})

test_that("removeGEO = FALSE returns an sf object", {
  layers <- prepare_district_layers(list(Council = fixture_layer_ns()))
  res <- AssignToDistricts(fixture_geocoded(), layers, removeGEO = FALSE, verbose = FALSE)
  expect_s3_class(res, "sf")
})

test_that("summary_stats reflect the geocode statuses", {
  layers <- prepare_district_layers(list(Council = fixture_layer_ns()))
  res <- AssignToDistricts(fixture_geocoded(), layers, verbose = FALSE)
  stats <- attr(res, "summary_stats")

  expect_equal(stats$original_count, 4)
  expect_equal(stats$successful_count, 2)
  expect_equal(stats$invalid_address_count, 1)
  expect_equal(stats$failed_geocoding_count, 1)
  expect_equal(stats$failed_geocoding_rows$original_row_id, 4)

  expect_output(print_summary(res), "Total addresses: 4")
})

test_that("AssignToDistricts validates its input", {
  layers <- prepare_district_layers(list(Council = fixture_layer_ns()))

  expect_error(
    AssignToDistricts(data.frame(x = 1), layers, verbose = FALSE),
    "missing column"
  )

  bad <- fixture_geocoded()
  bad$original_row_id <- c(1, 1, 2, 3)
  expect_error(
    AssignToDistricts(bad, layers, verbose = FALSE),
    "must be unique"
  )

  expect_error(
    AssignToDistricts(fixture_geocoded(), list(), verbose = FALSE),
    "At least one district layer"
  )
})

test_that("boundary points match with the default join, not with 'within'", {
  layers <- prepare_district_layers(list(Council = fixture_layer_ns()))
  # A point exactly ON the shared edge between South and North (lat 30.27).
  on_edge <- data.frame(
    Name = "edge", Street.Address = "x", City = "y",
    original_row_id = 1L, geocode_status = "OK",
    geo_x = -97.7, geo_y = 30.27
  )

  # default (intersects): the boundary point is assigned; the tie between the
  # two touching districts resolves to the first match with a message
  expect_message(
    res <- AssignToDistricts(on_edge, layers),
    "fell within multiple polygons"
  )
  expect_equal(res$Council_DISTRICT, "South")

  # strict within: boundary points are NOT inside either polygon
  res <- AssignToDistricts(on_edge, layers, join_type = "within", verbose = FALSE)
  expect_true(is.na(res$Council_DISTRICT))

  # interior points are identical under both join types
  interior <- fixture_geocoded()
  res_i <- AssignToDistricts(interior, layers, verbose = FALSE)
  res_w <- AssignToDistricts(interior, layers, join_type = "within", verbose = FALSE)
  expect_equal(res_i$Council_DISTRICT, res_w$Council_DISTRICT)
})

test_that("layers in a projected CRS still receive WGS84 points", {
  projected <- sf::st_transform(fixture_layer_ns(), 3081) # NAD83 / Texas State Mapping System
  layers <- prepare_district_layers(list(Council = projected))
  res <- AssignToDistricts(fixture_geocoded(), layers, verbose = FALSE)
  expect_equal(res$Council_DISTRICT, c("South", "North", NA, NA))
})

test_that("input columns colliding with layer output columns are renamed", {
  layers <- prepare_district_layers(list(Council = fixture_layer_ns()))
  stale <- fixture_geocoded()
  stale$Council_DISTRICT <- "stale value"   # e.g. a re-uploaded previous export

  expect_message(
    res <- AssignToDistricts(stale, layers),
    "renamed"
  )
  # fresh assignment keeps the canonical name; the stale copy is suffixed
  expect_equal(res$Council_DISTRICT, c("South", "North", NA, NA))
  expect_equal(res$Council_DISTRICT_input, rep("stale value", 4))
})

test_that("summary distinguishes geocoder errors and caps the failure listing", {
  layers <- prepare_district_layers(list(Council = fixture_layer_ns()))
  g <- data.frame(
    Street.Address = paste(1:12, "Nowhere Rd"),
    City = "Austin",
    original_row_id = 1:12,
    geocode_status = c(rep("No geocode match", 11), "Geocoder error"),
    geo_x = NA_real_, geo_y = NA_real_
  )
  res <- AssignToDistricts(g, layers, verbose = FALSE)
  stats <- attr(res, "summary_stats")

  expect_equal(stats$failed_geocoding_count, 12)
  expect_equal(stats$geocoder_error_count, 1)
  expect_output(print_summary(res), "and 2 more")
  expect_output(print_summary(res), "geocoder request errors")
})
