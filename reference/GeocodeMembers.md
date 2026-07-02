# Geocode a member list

Geocode a member list ONCE and return every input row, flagged and
(where possible) coordinated. No rows are dropped. Splitting geocoding
from assignment is what enables the "geocode once, apply to many
districting files" workflow: geocode a member list a single time, then
reuse those points across any number of layer sets via
[`AssignToDistricts()`](https://aberuiz.github.io/DistrictAssignments/reference/AssignToDistricts.md).

## Usage

``` r
GeocodeMembers(
  memberList,
  StreetCol,
  CityCol,
  boundaries = NULL,
  censusFallback = FALSE,
  verbose = TRUE
)
```

## Arguments

- memberList:

  A data.frame, or a path to a `.csv` / `.xlsx` / `.xls` file, of
  addresses (Excel files require the suggested `readxl` package).

- StreetCol:

  Name of the street-address column.

- CityCol:

  Name of the city column.

- boundaries:

  Optional bbox (see
  [`compute_search_extent()`](https://aberuiz.github.io/DistrictAssignments/reference/compute_search_extent.md))
  to restrict geocoding. Default `NULL` = unrestricted.

- censusFallback:

  If `TRUE`, addresses the primary (ArcGIS) geocoder fails on are
  retried against the free US Census Bureau geocoder (US addresses
  only). Recovered rows get coordinates, status `"OK"`, and
  `geo_source = "Census"`. Default `FALSE`.

- verbose:

  Print progress to the console.

## Value

The member data.frame with these columns added/replaced:

- original_row_id:

  stable 1..N identifier for mapping results back

- Street.Address, City:

  the resolved address/city used for geocoding

- geocode_status:

  `"OK"`, `"Missing address"` (blank street/city), `"No geocode match"`
  (the geocoder found no candidate), or `"Geocoder error"` (the request
  failed – e.g. a network/service problem – so the address was never
  actually tried)

- geo_x, geo_y:

  longitude/latitude (`NA` where not geocoded)

- geo_source:

  which service located the row: `"ArcGIS"`, `"Census"` (fallback), or
  `NA` if not geocoded

- geo\_\*:

  additional geocoder fields (geo_score, geo_match_addr, ...)

## Details

The columns `original_row_id`, `geocode_status`, and everything prefixed
`geo_` are reserved: they are (re)written by this function. If the input
already contains them (e.g. a re-uploaded previous export), they are
replaced with freshly computed values so stale results can never leak
into a new run.

The address and city columns are coerced to character (so a column read
as numeric still geocodes), and requests are sent in chunks of 500 so
that on large lists a transient service failure only affects its own
chunk: those rows are flagged `"Geocoder error"` (with a warning naming
the row range) instead of aborting the run, and can be recovered by
re-running or by the census fallback.

## Examples

``` r
if (FALSE) { # \dontrun{
pts <- GeocodeMembers("members.csv", "Street.Address", "City")
} # }
```
