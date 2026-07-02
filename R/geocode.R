# Geocoding a member list.

# Read a member list from disk: .csv natively, .xlsx/.xls via the suggested
# readxl package. Returns a plain data.frame either way.
read_member_file <- function(path) {
  ext <- tolower(tools::file_ext(path))
  if (ext %in% c("xlsx", "xls")) {
    if (!requireNamespace("readxl", quietly = TRUE)) {
      stop("Package 'readxl' is required to read Excel files. Install it with install.packages(\"readxl\").")
    }
    as.data.frame(readxl::read_excel(path))
  } else {
    utils::read.csv(path)
  }
}

#' Geocode a member list
#'
#' Geocode a member list ONCE and return every input row, flagged and (where
#' possible) coordinated. No rows are dropped. Splitting geocoding from
#' assignment is what enables the "geocode once, apply to many districting
#' files" workflow: geocode a member list a single time, then reuse those
#' points across any number of layer sets via [AssignToDistricts()].
#'
#' The columns `original_row_id`, `geocode_status`, and everything prefixed
#' `geo_` are reserved: they are (re)written by this function. If the input
#' already contains them (e.g. a re-uploaded previous export), they are
#' replaced with freshly computed values so stale results can never leak into
#' a new run.
#'
#' @param memberList A data.frame, or a path to a `.csv` / `.xlsx` / `.xls`
#'   file, of addresses (Excel files require the suggested `readxl` package).
#' @param StreetCol Name of the street-address column.
#' @param CityCol Name of the city column.
#' @param boundaries Optional bbox (see [compute_search_extent()]) to restrict
#'   geocoding. Default `NULL` = unrestricted.
#' @param verbose Print progress to the console.
#'
#' @return The member data.frame with these columns added/replaced:
#' \describe{
#'   \item{original_row_id}{stable 1..N identifier for mapping results back}
#'   \item{Street.Address, City}{the resolved address/city used for geocoding}
#'   \item{geocode_status}{`"OK"`, `"Missing address"`, or `"No geocode match"`}
#'   \item{geo_x, geo_y}{longitude/latitude (`NA` where not geocoded)}
#'   \item{geo_*}{additional geocoder fields (geo_score, geo_match_addr, ...)}
#' }
#'
#' @examples
#' \dontrun{
#' pts <- GeocodeMembers("members.csv", "Street.Address", "City")
#' }
#' @export
GeocodeMembers <- function(memberList, StreetCol, CityCol, boundaries = NULL, verbose = TRUE) {
  if (is.character(memberList) && length(memberList) == 1) {
    memberList <- read_member_file(memberList)
  }

  missing_cols <- setdiff(c(StreetCol, CityCol), names(memberList))
  if (length(missing_cols) > 0) {
    stop(sprintf(
      "Column(s) not found in the member list: %s. Available columns: %s",
      paste(missing_cols, collapse = ", "),
      paste(names(memberList), collapse = ", ")
    ))
  }

  # Reserved output columns: drop any pre-existing geocode results (a
  # re-uploaded prior export) so a row that fails THIS run can't silently keep
  # coordinates or a status from a previous one.
  stale <- names(memberList)[names(memberList) == "geocode_status" |
                               startsWith(names(memberList), "geo_")]
  if (length(stale) > 0) {
    if (verbose) {
      cat(paste0("Replacing previous geocode column(s) from the input: ",
                 paste(stale, collapse = ", "), "\n"))
    }
    memberList <- memberList[, setdiff(names(memberList), stale), drop = FALSE]
  }

  memberList$Street.Address <- memberList[[StreetCol]]
  memberList$City <- memberList[[CityCol]]

  # Reserved identifier, always rewritten: downstream row alignment and
  # de-duplication assume it is unique and 1..N.
  memberList$original_row_id <- seq_len(nrow(memberList))

  # Row count and identifiers captured up front. memberList is only ever assigned
  # INTO from here (never subset or reordered), so these must still hold at the
  # end -- see the invariant check below. This is the backbone of "absolutely
  # match rows".
  n_input <- nrow(memberList)
  original_row_id_before <- memberList$original_row_id

  # Flag rows whose street or city is blank -- NA, empty, or whitespace-only.
  # They can't be geocoded but stay in the table with status "Missing address".
  street_blank <- is.na(memberList$Street.Address) | trimws(memberList$Street.Address) == ""
  city_blank   <- is.na(memberList$City)           | trimws(memberList$City) == ""
  valid_addresses <- !street_blank & !city_blank

  memberList$geocode_status <- ifelse(valid_addresses, "OK", "Missing address")
  if (verbose) cat(paste("Number of geocodable addresses:", sum(valid_addresses), "\n"))

  valid_idx <- which(valid_addresses)

  if (length(valid_idx) > 0) {
    if (verbose) cat("Geocoding addresses...\n")

    args <- list(
      address = memberList$Street.Address[valid_idx],
      city = memberList$City[valid_idx],
      max_locations = 1
    )
    if (!is.null(boundaries)) args$search_extent <- boundaries
    memberGeos <- do.call(arcgisgeocode::find_address_candidates, args)

    if (verbose) cat(paste("Number of geocoded results:", nrow(memberGeos), "\n"))

    # Carry every geocode field back onto the full member list, keyed by the
    # input row each candidate came from. find_address_candidates() tags every
    # returned candidate with `input_id`, a 1-based index into the addresses we
    # sent (in the package source the results are cbind()ed with
    # input_id = rep(seq_along(inputs), n_candidates_per_input)). Using it places
    # each result on the EXACT source row instead of binding by position -- which
    # matters because inputs that return NO candidate are simply absent from the
    # response, so a positional bind would shift every following row.
    #
    # NOTE: do NOT key on `result_id`. That is a field of the *batch*
    # geocode_addresses() endpoint; here it appears among the geocoder's
    # attribute columns as all-NA (it is only populated when you pass input IDs),
    # so keying on it drops every row -> "nothing geocoded".
    #
    # Geocoder fields are prefixed with "geo_" (geo_x, geo_y, geo_score, ...) so
    # they can never overwrite a same-named column in the member CSV (a CSV with
    # its own "x"/"y"/"score" column would otherwise be clobbered).
    geo_df <- if (inherits(memberGeos, "sf")) sf::st_drop_geometry(memberGeos) else memberGeos

    if ("input_id" %in% names(geo_df)) {
      input_ids <- suppressWarnings(as.integer(geo_df[["input_id"]]))
    } else if (nrow(geo_df) == length(valid_idx)) {
      # Fall back to positional alignment only when counts match exactly.
      input_ids <- seq_len(nrow(geo_df))
    } else {
      stop("Geocoder response is missing 'input_id' and the row count is unexpected; cannot align results safely.")
    }

    # Keep the first candidate per input (max_locations = 1 already limits to one,
    # but guard against repeats), and only ids that point to a real input row;
    # anything else is left NA and flagged "No geocode match" below.
    sel <- !duplicated(input_ids) &
           !is.na(input_ids) &
           input_ids >= 1 &
           input_ids <= length(valid_idx)
    target_rows <- valid_idx[input_ids[sel]]

    geo_cols <- setdiff(names(geo_df), c("input_id", "result_id"))
    for (col in geo_cols) {
      # Skip list-columns (e.g. the geocoder's nested `extents`): they can't be
      # written to CSV and aren't useful per-member fields.
      if (!is.atomic(geo_df[[col]])) next
      out_col <- paste0("geo_", col)
      memberList[[out_col]] <- NA
      memberList[[out_col]][target_rows] <- geo_df[[col]][sel]
    }
  } else if (verbose) {
    cat("No geocodable addresses found.\n")
  }

  # Guarantee coordinate columns exist even if geocoding produced none.
  if (is.null(memberList$geo_x)) memberList$geo_x <- NA_real_
  if (is.null(memberList$geo_y)) memberList$geo_y <- NA_real_

  # Flag address-valid rows that still failed to geocode (no candidate returned).
  failed_geo <- valid_addresses & (is.na(memberList$geo_x) | is.na(memberList$geo_y))
  memberList$geocode_status[failed_geo] <- "No geocode match"

  if (verbose) {
    cat(paste("Number of geocoded addresses:",
              sum(!is.na(memberList$geo_x) & !is.na(memberList$geo_y)), "\n"))
  }

  # Invariant: geocoding never adds, drops, or reorders member rows -- every
  # input row is returned at its original position, matched to its result or NA.
  # Also, every "OK" row must carry coordinates and every non-"OK" row must not,
  # so the status flag and the data can never disagree.
  stopifnot(
    nrow(memberList) == n_input,
    identical(memberList$original_row_id, original_row_id_before),
    all((memberList$geocode_status == "OK") ==
          (!is.na(memberList$geo_x) & !is.na(memberList$geo_y)))
  )

  memberList
}
