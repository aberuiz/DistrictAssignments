# Geocoding a member list.

# Fallback geocoder: the US Census Bureau's free onelineaddress endpoint.
# Sequential single requests -- this only ever runs on the rows the primary
# geocoder failed on, so batch plumbing isn't worth its complexity.
# Returns one row per input with x/y/matched_address, NA where no match; a
# request error just leaves its row NA (the row simply stays failed). If the
# service itself is unreachable (several requests in a row error, as opposed to
# merely not matching), the loop stops early instead of burning a 30-second
# timeout on every remaining row of a large failed batch.
geocode_census_oneline <- function(street, city, verbose = TRUE) {
  out <- data.frame(
    x = rep(NA_real_, length(street)),
    y = NA_real_,
    matched_address = NA_character_
  )
  base_req <- httr2::request("https://geocoding.geo.census.gov/geocoder/locations/onelineaddress") |>
    httr2::req_user_agent("DistrictAssignments R package (https://github.com/aberuiz/DistrictAssignments)") |>
    httr2::req_timeout(30)

  consecutive_errors <- 0L
  for (i in seq_along(street)) {
    if (verbose && length(street) > 100 && i %% 100 == 0) {
      cat(sprintf("Census fallback: %d of %d...\n", i, length(street)))
    }
    request_ok <- tryCatch({
      resp <- base_req |>
        httr2::req_url_query(
          address = paste(street[i], city[i], sep = ", "),
          benchmark = "Public_AR_Current",
          format = "json"
        ) |>
        httr2::req_perform()
      body <- yyjsonr::read_json_str(httr2::resp_body_string(resp))
      # yyjsonr simplifies the addressMatches array of objects into a
      # data.frame (one row per candidate) whose `coordinates` field is a
      # list-column; an empty array comes back as a zero-length list.
      matches <- body$result$addressMatches
      if (is.data.frame(matches) && nrow(matches) > 0) {
        coords <- matches$coordinates[[1]]
        out$x[i] <- as.numeric(coords$x)
        out$y[i] <- as.numeric(coords$y)
        out$matched_address[i] <- as.character(matches$matchedAddress[1])
      }
      TRUE
    }, error = function(e) {
      if (verbose) cat(sprintf("Census fallback request failed for row %d: %s\n", i, conditionMessage(e)))
      FALSE
    })

    if (request_ok) {
      consecutive_errors <- 0L
    } else {
      consecutive_errors <- consecutive_errors + 1L
      if (consecutive_errors >= 5L && i < length(street)) {
        warning(sprintf(
          "Census fallback stopped after %d consecutive request failures; %d remaining address(es) left ungeocoded.",
          consecutive_errors, length(street) - i
        ), call. = FALSE)
        break
      }
    }
  }
  out
}

# Primary geocoder call, split into chunks so one bad request can't sink a
# whole run. arcgisgeocode::find_address_candidates() sends one HTTP request
# per address and aborts the ENTIRE call if any response errors, so on a list
# of thousands of rows a single transient failure would otherwise lose
# everything. A failed chunk raises a warning and its rows simply stay
# ungeocoded ("No geocode match"), where the census fallback can still retry
# them. Returns a plain data.frame (geometry dropped, atomic columns only)
# whose `input_id` indexes into the FULL street/city vectors.
geocode_arcgis_chunked <- function(street, city, boundaries = NULL,
                                   verbose = TRUE, chunk_size = 500L) {
  n <- length(street)
  starts <- seq.int(1L, n, by = chunk_size)
  pieces <- vector("list", length(starts))
  # Full-vector indices whose REQUEST failed (vs. simply not matching); the
  # caller flags them "Geocoder error" so a service hiccup is never mistaken
  # for a bad address. Returned as the "error_input_ids" attribute.
  error_ids <- integer(0)

  for (k in seq_along(starts)) {
    idx <- starts[k]:min(starts[k] + chunk_size - 1L, n)
    if (verbose && length(starts) > 1) {
      cat(sprintf("Geocoding rows %d-%d of %d...\n", idx[1], idx[length(idx)], n))
    }
    args <- list(address = street[idx], city = city[idx], max_locations = 1)
    if (!is.null(boundaries)) args$search_extent <- boundaries

    res <- tryCatch(
      do.call(arcgisgeocode::find_address_candidates, args),
      error = function(e) {
        warning(sprintf(
          "Geocoding rows %d-%d failed (%s); they are flagged 'Geocoder error'.",
          idx[1], idx[length(idx)], conditionMessage(e)
        ), call. = FALSE)
        NULL
      }
    )
    if (is.null(res)) {
      error_ids <- c(error_ids, idx)
      next
    }
    if (inherits(res, "sf")) res <- sf::st_drop_geometry(res)
    # Drop list-columns (e.g. the geocoder's nested `extents`) up front: they
    # can't be rbind()ed reliably across chunks and are skipped downstream anyway.
    res <- res[, vapply(res, is.atomic, logical(1)), drop = FALSE]

    # Align results to their source rows via input_id (see the NOTE in
    # GeocodeMembers about why input_id, never result_id). Inputs that return
    # no candidate are simply absent from the response, so a positional
    # fallback is only safe when the counts match exactly.
    if (!"input_id" %in% names(res)) {
      if (nrow(res) == length(idx)) {
        res$input_id <- seq_along(idx)
      } else {
        stop("Geocoder response is missing 'input_id' and the row count is unexpected; cannot align results safely.")
      }
    }
    # input_id is 1-based within the chunk; shift it to the full-vector index.
    res$input_id <- suppressWarnings(as.integer(res$input_id)) + idx[1] - 1L
    pieces[[k]] <- res
  }

  pieces <- pieces[!vapply(pieces, is.null, logical(1))]
  out <- if (length(pieces) == 0) {
    data.frame(input_id = integer(0))
  } else {
    # rbind chunks, filling any column a chunk happens to lack with NA.
    all_cols <- unique(unlist(lapply(pieces, names)))
    pieces <- lapply(pieces, function(p) {
      for (col in setdiff(all_cols, names(p))) p[[col]] <- NA
      p[, all_cols, drop = FALSE]
    })
    do.call(rbind, pieces)
  }
  attr(out, "error_input_ids") <- error_ids
  out
}

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
#' The address and city columns are coerced to character (so a column read as
#' numeric still geocodes), and requests are sent in chunks of 500 so that on
#' large lists a transient service failure only affects its own chunk: those
#' rows are flagged `"Geocoder error"` (with a warning naming the row range)
#' instead of aborting the run, and can be recovered by re-running or by the
#' census fallback.
#'
#' @param memberList A data.frame, or a path to a `.csv` / `.xlsx` / `.xls`
#'   file, of addresses (Excel files require the suggested `readxl` package).
#' @param StreetCol Name of the street-address column.
#' @param CityCol Name of the city column.
#' @param boundaries Optional bbox (see [compute_search_extent()]) to restrict
#'   geocoding. Default `NULL` = unrestricted.
#' @param censusFallback If `TRUE`, addresses the primary (ArcGIS) geocoder
#'   fails on are retried against the free US Census Bureau geocoder (US
#'   addresses only). Recovered rows get coordinates, status `"OK"`, and
#'   `geo_source = "Census"`. Default `FALSE`.
#' @param verbose Print progress to the console.
#'
#' @return The member data.frame with these columns added/replaced:
#' \describe{
#'   \item{original_row_id}{stable 1..N identifier for mapping results back}
#'   \item{Street.Address, City}{the resolved address/city used for geocoding}
#'   \item{geocode_status}{`"OK"`, `"Missing address"` (blank street/city),
#'     `"No geocode match"` (the geocoder found no candidate), or
#'     `"Geocoder error"` (the request failed -- e.g. a network/service
#'     problem -- so the address was never actually tried)}
#'   \item{geo_x, geo_y}{longitude/latitude (`NA` where not geocoded)}
#'   \item{geo_source}{which service located the row: `"ArcGIS"`, `"Census"`
#'     (fallback), or `NA` if not geocoded}
#'   \item{geo_*}{additional geocoder fields (geo_score, geo_match_addr, ...)}
#' }
#'
#' @examples
#' \dontrun{
#' pts <- GeocodeMembers("members.csv", "Street.Address", "City")
#' }
#' @export
GeocodeMembers <- function(memberList, StreetCol, CityCol, boundaries = NULL,
                           censusFallback = FALSE, verbose = TRUE) {
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

  # Coerce to character: a street or city column that Excel/CSV read as
  # numeric (house numbers, ZIP-like values) would otherwise abort the whole
  # run inside the geocoder's type check.
  memberList$Street.Address <- as.character(memberList[[StreetCol]])
  memberList$City <- as.character(memberList[[CityCol]])

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
  # Full-list row indices whose geocode REQUEST failed (chunk error); flagged
  # "Geocoder error" below so they are distinguishable from true non-matches.
  error_rows <- integer(0)

  if (length(valid_idx) > 0) {
    if (verbose) cat("Geocoding addresses...\n")

    # Sent in chunks so one failed request can't abort the whole run (see
    # geocode_arcgis_chunked); a failed chunk warns and its rows stay NA.
    memberGeos <- geocode_arcgis_chunked(
      memberList$Street.Address[valid_idx],
      memberList$City[valid_idx],
      boundaries = boundaries,
      verbose = verbose
    )
    error_rows <- valid_idx[attr(memberGeos, "error_input_ids")]

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
    # geocode_arcgis_chunked already dropped geometry/list-columns and
    # guarantees an input_id column (indexing into the addresses we sent).
    geo_df <- memberGeos
    input_ids <- geo_df[["input_id"]]

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

  # Rows whose geocode request itself failed are a different situation from
  # rows the geocoder examined and couldn't match: "Geocoder error" means the
  # address was never really tried and a re-run may succeed; "No geocode
  # match" means the address itself didn't resolve. Keeps a service hiccup
  # from reading as thousands of "bad addresses".
  if (length(error_rows) > 0) {
    memberList$geocode_status[error_rows] <- "Geocoder error"
  }

  # Which service located each row (NA where not geocoded). Set from the
  # primary pass first; the fallback below overwrites its recovered rows.
  memberList$geo_source <- ifelse(memberList$geocode_status == "OK", "ArcGIS", NA_character_)

  # Optional second chance: retry the failed rows (both "No geocode match"
  # and "Geocoder error") against the US Census geocoder. Keyed by row index,
  # so alignment works exactly like the primary pass; rows the fallback also
  # misses simply keep their failure status.
  if (isTRUE(censusFallback) && any(failed_geo)) {
    idx <- which(failed_geo)
    if (verbose) cat(sprintf("Retrying %d failed address(es) with the Census geocoder...\n", length(idx)))

    cen <- geocode_census_oneline(memberList$Street.Address[idx],
                                  memberList$City[idx], verbose = verbose)
    got <- !is.na(cen$x) & !is.na(cen$y)
    rows <- idx[got]
    if (length(rows) > 0) {
      memberList$geo_x[rows] <- cen$x[got]
      memberList$geo_y[rows] <- cen$y[got]
      if (is.null(memberList$geo_match_addr)) memberList$geo_match_addr <- NA_character_
      memberList$geo_match_addr[rows] <- cen$matched_address[got]
      memberList$geo_source[rows] <- "Census"
      memberList$geocode_status[rows] <- "OK"
    }
    if (verbose) cat(sprintf("Census fallback recovered %d address(es).\n", length(rows)))
  }

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
