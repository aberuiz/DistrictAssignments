# Offline behavior of GeocodeMembers (no geocodable rows -> no network call),
# plus one live round-trip that is skipped without a connection.

test_that("blank addresses are flagged without calling the geocoder", {
  members <- data.frame(
    Name = c("A", "B", "C"),
    Street.Address = c("", NA, "   "),
    City = c("", "Austin", "Austin")
  )
  pts <- GeocodeMembers(members, "Street.Address", "City", verbose = FALSE)

  expect_equal(pts$geocode_status, rep("Missing address", 3))
  expect_equal(pts$original_row_id, 1:3)
  expect_true(all(is.na(pts$geo_x)))
  expect_true(all(is.na(pts$geo_y)))
})

test_that("missing street/city columns error clearly", {
  members <- data.frame(Name = "A", Addr = "x", Town = "y")
  expect_error(
    GeocodeMembers(members, "Street.Address", "City", verbose = FALSE),
    "not found in the member list"
  )
})

test_that("stale geocode columns from a previous export are replaced", {
  members <- data.frame(
    Name = c("A", "B"),
    Street.Address = c("", ""),
    City = c("", ""),
    geocode_status = c("OK", "OK"),
    geo_x = c(-1, -2),
    geo_y = c(-1, -2),
    original_row_id = c(9, 9)
  )
  pts <- GeocodeMembers(members, "Street.Address", "City", verbose = FALSE)

  # stale coordinates and statuses must not survive
  expect_true(all(is.na(pts$geo_x)))
  expect_equal(pts$geocode_status, rep("Missing address", 2))
  # reserved id is rewritten to a unique 1..N sequence
  expect_equal(pts$original_row_id, 1:2)
})

test_that("member lists can be read from Excel files", {
  skip_if_not_installed("readxl")
  skip_if_not_installed("writexl")

  members <- data.frame(
    Name = c("A", "B"),
    Street.Address = c("", ""),
    City = c("", "")
  )
  path <- tempfile(fileext = ".xlsx")
  writexl::write_xlsx(members, path)

  pts <- GeocodeMembers(path, "Street.Address", "City", verbose = FALSE)
  expect_equal(pts$Name, c("A", "B"))
  expect_equal(pts$geocode_status, rep("Missing address", 2))
})

test_that("census fallback fills recovered rows and sets geo_source (mocked)", {
  # Primary geocoder finds nothing; Census recovers the first row only.
  local_mocked_bindings(
    find_address_candidates = function(...) {
      data.frame(input_id = integer(0), x = numeric(0), y = numeric(0))
    },
    .package = "arcgisgeocode"
  )
  local_mocked_bindings(
    geocode_census_oneline = function(street, city, verbose = TRUE) {
      data.frame(x = c(-97.7, NA), y = c(30.2, NA),
                 matched_address = c("100 MAIN ST, AUSTIN, TX", NA))
    }
  )

  members <- data.frame(Street.Address = c("100 Main St", "999 Void Rd"),
                        City = c("Austin", "Austin"))
  pts <- GeocodeMembers(members, "Street.Address", "City",
                        censusFallback = TRUE, verbose = FALSE)

  expect_equal(pts$geocode_status, c("OK", "No geocode match"))
  expect_equal(pts$geo_source, c("Census", NA))
  expect_equal(pts$geo_x, c(-97.7, NA))
  expect_equal(pts$geo_match_addr, c("100 MAIN ST, AUSTIN, TX", NA))

  # without the fallback, both rows stay failed
  pts2 <- GeocodeMembers(members, "Street.Address", "City", verbose = FALSE)
  expect_equal(pts2$geocode_status, rep("No geocode match", 2))
  expect_true(all(is.na(pts2$geo_source)))
})

test_that("census fallback recovers rows the primary geocoder missed", {
  skip_if_not_installed("curl")
  skip_if_offline("geocoding.geo.census.gov")

  # Call the fallback helper directly: one findable address, one hopeless one.
  cen <- geocode_census_oneline(
    c("100 Congress Ave", "zzzzqqqq nonexistent 99999 xx"),
    c("Austin", "Qqzzville"),
    verbose = FALSE
  )
  expect_equal(nrow(cen), 2)
  expect_true(abs(cen$x[1] - (-97.74)) < 0.1)
  expect_true(abs(cen$y[1] - 30.26) < 0.1)
  expect_true(is.na(cen$x[2]))
})

test_that("live geocoding aligns results to their source rows", {
  skip_if_not_installed("curl")
  skip_if_offline("geocode.arcgis.com")

  members <- data.frame(
    Name = c("Good1", "Blank", "Bad", "Good2"),
    Street.Address = c("100 Congress Ave", "", "zzzzqqqq nonexistent 99999 xx", "1100 Congress Ave"),
    City = c("Austin", "Austin", "Qqzzville", "Austin")
  )
  pts <- suppressWarnings(
    GeocodeMembers(members, "Street.Address", "City", verbose = FALSE)
  )

  expect_equal(pts$original_row_id, 1:4)
  expect_equal(pts$geocode_status,
               c("OK", "Missing address", "No geocode match", "OK"))
  # both located points are in Austin, and the failed rows carry no coordinates
  expect_true(all(abs(pts$geo_x[c(1, 4)] - (-97.74)) < 0.1))
  expect_true(all(is.na(pts$geo_x[2:3])))
  # the geocoder's nested list-columns are not carried through
  expect_true(all(vapply(pts, is.atomic, logical(1))))
})

test_that("numeric street/city columns are coerced, not fatal", {
  seen <- NULL
  local_mocked_bindings(
    find_address_candidates = function(address, city, ...) {
      seen <<- list(address = address, city = city)
      data.frame(input_id = integer(0))
    },
    .package = "arcgisgeocode"
  )
  # e.g. an Excel sheet whose address column came in as numbers
  members <- data.frame(S = c(101, 102), C = c(78701, 78702))
  pts <- GeocodeMembers(members, "S", "C", verbose = FALSE)

  expect_true(is.character(seen$address))
  expect_true(is.character(seen$city))
  expect_equal(pts$geocode_status, rep("No geocode match", 2))
})

test_that("a failed geocode chunk only loses its own rows", {
  local_mocked_bindings(
    find_address_candidates = function(address, city, ...) {
      if (any(address == "BOOM")) stop("simulated server error")
      data.frame(input_id = seq_along(address),
                 x = -97 - seq_along(address), y = 30 + seq_along(address))
    },
    .package = "arcgisgeocode"
  )
  street <- c("a", "b", "BOOM", "d", "e")
  expect_warning(
    res <- geocode_arcgis_chunked(street, rep("Austin", 5),
                                  verbose = FALSE, chunk_size = 2L),
    "Geocoding rows 3-4 failed"
  )
  # chunk 2 (rows 3-4) failed; surviving results keep their GLOBAL input ids
  expect_equal(res$input_id, c(1L, 2L, 5L))
  expect_equal(attr(res, "error_input_ids"), 3:4)
})

test_that("request failures are flagged 'Geocoder error', recoverable by the fallback", {
  local_mocked_bindings(
    find_address_candidates = function(...) stop("simulated outage"),
    .package = "arcgisgeocode"
  )
  members <- data.frame(Street.Address = c("100 Main St", "", "200 Oak St"),
                        City = c("Austin", "Austin", "Austin"))

  expect_warning(
    pts <- GeocodeMembers(members, "Street.Address", "City", verbose = FALSE),
    "Geocoder error"
  )
  expect_equal(pts$geocode_status,
               c("Geocoder error", "Missing address", "Geocoder error"))
  expect_true(all(is.na(pts$geo_x)))

  # the census fallback still retries errored rows and can recover them
  local_mocked_bindings(
    geocode_census_oneline = function(street, city, verbose = TRUE) {
      data.frame(x = c(-97.7, NA), y = c(30.2, NA),
                 matched_address = c("100 MAIN ST, AUSTIN, TX", NA))
    }
  )
  expect_warning(
    pts2 <- GeocodeMembers(members, "Street.Address", "City",
                           censusFallback = TRUE, verbose = FALSE),
    "Geocoder error"
  )
  expect_equal(pts2$geocode_status,
               c("OK", "Missing address", "Geocoder error"))
  expect_equal(pts2$geo_source, c("Census", NA, NA))
})
