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
