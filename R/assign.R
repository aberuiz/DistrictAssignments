# Assigning geocoded points to district layers.

# Collapse a spatial-join result back to one row per input point. st_join emits
# one row per polygon a point falls inside, so a point landing in overlapping
# polygons would multiply into several rows (and compound across layers). We keep
# the FIRST match per id and note how many points were affected.
keep_first_match <- function(joined, id_col = "original_row_id", verbose = TRUE) {
  dup <- duplicated(joined[[id_col]])
  if (any(dup) && verbose) {
    n_multi <- length(unique(joined[[id_col]][dup]))
    message(sprintf(
      "%d point(s) fell within multiple polygons in a layer; keeping the first match.",
      n_multi
    ))
  }
  joined[!dup, , drop = FALSE]
}

#' Assign geocoded points to district layers
#'
#' Assign already-geocoded points (the output of [GeocodeMembers()]) to one or
#' more PREPARED district layers (the output of [prepare_district_layers()]).
#' This function does not prepare/prefix layers itself, so callers reusing
#' geocoded points across many layer sets pay preparation only once.
#'
#' Every input row is carried through: rows without coordinates become empty
#' points and simply receive `NA` district columns. Supports any number of
#' layers; overlapping polygons within a layer resolve to the first match (with
#' a message) so a point is never duplicated into multiple rows.
#'
#' @param geocoded Output of [GeocodeMembers()] (all rows, with `geo_x`/`geo_y`
#'   and `geocode_status`).
#' @param layers A list of prepared sf layers (see [prepare_district_layers()]).
#' @param removeGEO Drop the geometry column from the result (`TRUE` = clean
#'   CSV export).
#' @param join_type How a point must relate to a polygon to be assigned.
#'   `"intersects"` (default) also matches points lying exactly ON a district
#'   boundary (a boundary point touching two districts resolves to the first,
#'   with a message); `"within"` requires the point to be strictly inside, so
#'   boundary points receive `NA`.
#' @param verbose Print messages about multiply-matched points.
#'
#' @return A data.frame (or sf object if `removeGEO = FALSE`) with one row per
#'   input member, in the original order, with the district layers' prefixed
#'   columns joined on. Summary statistics are attached as the
#'   `"summary_stats"` attribute; see [print_summary()].
#'
#' @examples
#' \dontrun{
#' pts    <- GeocodeMembers("members.csv", "Street.Address", "City")
#' layers <- prepare_district_layers("shapes/congress.shp", "Congressional")
#' result <- AssignToDistricts(pts, layers)
#' }
#' @export
AssignToDistricts <- function(geocoded, layers, removeGEO = TRUE,
                              join_type = c("intersects", "within"),
                              verbose = TRUE) {
  if (!inherits(layers, "list")) layers <- list(layers)
  if (length(layers) == 0) stop("At least one district layer is required.")
  join_type <- match.arg(join_type)
  join_fn <- switch(join_type, intersects = sf::st_intersects, within = sf::st_within)

  required_cols <- c("original_row_id", "geo_x", "geo_y", "geocode_status")
  missing_cols <- setdiff(required_cols, names(geocoded))
  if (length(missing_cols) > 0) {
    stop(sprintf(
      "`geocoded` is missing column(s): %s. Pass the output of GeocodeMembers().",
      paste(missing_cols, collapse = ", ")
    ))
  }
  ids <- geocoded$original_row_id
  if (anyNA(ids) || anyDuplicated(ids)) {
    stop("`geocoded$original_row_id` must be unique and non-NA. Pass the output of GeocodeMembers().")
  }

  target_crs <- sf::st_crs(layers[[1]])

  # Joins run planar (s2 off) regardless of what was set before, so results
  # don't depend on session state; the caller's setting is restored after.
  old_s2 <- suppressMessages(sf::sf_use_s2(FALSE))
  on.exit(suppressMessages(sf::sf_use_s2(old_s2)))

  # Turn EVERY row into a point; NA coordinates become empty points (na.fail =
  # FALSE) that a left st_join keeps with NA district columns.
  member_pts <- geocoded |>
    sf::st_as_sf(
      coords = c("geo_x", "geo_y"),
      crs = sf::st_crs("EPSG:4326"),
      na.fail = FALSE
    ) |>
    sf::st_transform(crs = target_crs)

  # Join each layer in turn, de-duping to first match after each so overlapping
  # polygons can't multiply rows (and can't compound across layers).
  final <- member_pts
  for (i in seq_along(layers)) {
    layer <- sf::st_transform(layers[[i]], crs = target_crs)
    # suppressMessages: sf notes that the predicate treats lon/lat as planar,
    # which is exactly the (intentional) s2-off behavior set above.
    final <- suppressMessages(sf::st_join(final, layer, join = join_fn))
    final <- keep_first_match(final, "original_row_id", verbose = verbose)
  }

  # Invariant: after joining + first-match de-dup, the result has exactly one row
  # per input member, in the original order. Assignment never loses or multiplies
  # a member row.
  stopifnot(
    nrow(final) == nrow(geocoded),
    identical(final$original_row_id, geocoded$original_row_id)
  )

  result <- if (isTRUE(removeGEO)) sf::st_drop_geometry(final) else final

  # Summary counts come from the geocode_status flags on the (row-complete)
  # input, so they reflect input rows regardless of join behavior.
  invalid_count <- sum(geocoded$geocode_status == "Missing address")
  failed_count  <- sum(geocoded$geocode_status == "No geocode match")
  success_count <- sum(geocoded$geocode_status == "OK")

  status_cols <- intersect(c("original_row_id", "Street.Address", "City", "geocode_status"),
                           names(geocoded))
  invalid_rows <- geocoded[geocoded$geocode_status == "Missing address", status_cols]
  failed_rows  <- geocoded[geocoded$geocode_status == "No geocode match", status_cols]

  attr(result, "summary_stats") <- list(
    original_count = nrow(geocoded),
    invalid_address_count = invalid_count,
    invalid_rows = if (invalid_count > 0) invalid_rows else NULL,
    failed_geocoding_count = failed_count,
    failed_geocoding_rows = if (failed_count > 0) failed_rows else NULL,
    successful_count = success_count
  )

  result
}

#' Print the summary statistics of an assignment result
#'
#' Prints the summary attached to an [AssignToDistricts()] /
#' [AssignDistricts()] result to the console: total, invalid, failed, and
#' successful address counts, plus the specific rows that failed to geocode.
#'
#' @param result The return value of [AssignToDistricts()] or
#'   [AssignDistricts()].
#'
#' @return `result`, invisibly.
#' @export
print_summary <- function(result) {
  stats <- attr(result, "summary_stats")
  if (is.null(stats)) return(invisible(result))

  cat("\n=== SUMMARY ===\n")
  cat(sprintf("Total addresses: %d\n", stats$original_count))
  cat(sprintf("Invalid addresses (blank/NA): %d\n", stats$invalid_address_count))
  cat(sprintf("Failed geocoding: %d\n", stats$failed_geocoding_count))
  cat(sprintf("Successfully geocoded: %d\n", stats$successful_count))

  if (!is.null(stats$failed_geocoding_rows)) {
    cat("\nFailed addresses:\n")
    fr <- stats$failed_geocoding_rows
    for (i in seq_len(nrow(fr))) {
      cat(sprintf("  Row %d: %s, %s\n",
                  fr$original_row_id[i], fr$Street.Address[i], fr$City[i]))
    }
  }
  cat("===============\n\n")
  invisible(result)
}

#' Geocode a member list and assign it to districts in one call
#'
#' One-shot convenience wrapper around [prepare_district_layers()],
#' [GeocodeMembers()], and [AssignToDistricts()]. For the "geocode once, apply
#' to many districting files" workflow, call [GeocodeMembers()] once and then
#' [AssignToDistricts()] repeatedly instead.
#'
#' @inheritParams GeocodeMembers
#' @inheritParams prepare_district_layers
#' @inheritParams AssignToDistricts
#' @param restrictToDistrictArea If `TRUE`, geocoding is limited to the
#'   (padded) union extent of the district layers. Off by default -- see
#'   [compute_search_extent()] for why. Ignored if `boundaries` is supplied.
#' @param boundaries Optional explicit geocoding search extent (a bbox).
#'   Overrides `restrictToDistrictArea`. Default `NULL` = no restriction.
#' @param verbose Print progress and a summary to the console.
#'
#' @return See [AssignToDistricts()].
#'
#' @examples
#' \dontrun{
#' result <- AssignDistricts(
#'   memberList    = "members.csv",
#'   StreetCol     = "Street.Address",
#'   CityCol       = "City",
#'   districtsList = list(
#'     Congressional = "shapes/congress.shp",
#'     StateSenate   = "shapes/senate.geojson"
#'   )
#' )
#' }
#' @export
AssignDistricts <- function(memberList, StreetCol = "Street.Address", CityCol = "City",
                            districtsList, removeGEO = TRUE, districtNames = NULL,
                            restrictToDistrictArea = FALSE, boundaries = NULL,
                            join_type = c("intersects", "within"),
                            verbose = TRUE) {
  layers <- prepare_district_layers(districtsList, districtNames)

  # Geocoding is unrestricted by default; only build a search extent when the
  # caller explicitly asks for one (or supplies their own).
  if (is.null(boundaries) && isTRUE(restrictToDistrictArea)) {
    boundaries <- compute_search_extent(layers)
  }

  geocoded <- GeocodeMembers(memberList, StreetCol, CityCol, boundaries, verbose = verbose)
  result <- AssignToDistricts(geocoded, layers, removeGEO = removeGEO,
                              join_type = join_type, verbose = verbose)

  if (verbose) print_summary(result)

  result
}
