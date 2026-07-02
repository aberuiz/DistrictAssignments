# Shared fixtures: small synthetic layers around Austin, TX, and a fake
# geocoded member table, so the assignment pipeline can be tested offline.

mk_poly <- function(xmin, ymin, xmax, ymax) {
  sf::st_polygon(list(matrix(
    c(xmin, ymin, xmax, ymin, xmax, ymax, xmin, ymax, xmin, ymin),
    ncol = 2, byrow = TRUE
  )))
}

# Two boxes splitting the area north/south at latitude 30.27.
fixture_layer_ns <- function() {
  sf::st_sf(
    DISTRICT = c("South", "North"),
    geometry = sf::st_sfc(
      mk_poly(-98, 30.0, -97.5, 30.27),
      mk_poly(-98, 30.27, -97.5, 30.6),
      crs = 4326
    )
  )
}

# Two identical overlapping boxes covering everything (tests first-match de-dup).
fixture_layer_overlap <- function() {
  sf::st_sf(
    ZONE = c("Z1", "Z2"),
    geometry = sf::st_sfc(
      mk_poly(-99, 29, -97, 31),
      mk_poly(-99, 29, -97, 31),
      crs = 4326
    )
  )
}

# A geocoded member table shaped like GeocodeMembers() output: two located
# points (one in each NS district), one missing address, one failed geocode.
fixture_geocoded <- function() {
  data.frame(
    Name = c("Alice", "Bob", "Carol", "Dave"),
    Street.Address = c("a", "b", "", "d"),
    City = c("Austin", "Austin", "", "Austin"),
    original_row_id = 1:4,
    geocode_status = c("OK", "OK", "Missing address", "No geocode match"),
    geo_x = c(-97.74, -97.74, NA, NA),
    geo_y = c(30.26, 30.28, NA, NA),
    stringsAsFactors = FALSE
  )
}
