# Geocode a member list and assign it to districts in one call

One-shot convenience wrapper around
[`prepare_district_layers()`](https://aberuiz.github.io/DistrictAssignments/reference/prepare_district_layers.md),
[`GeocodeMembers()`](https://aberuiz.github.io/DistrictAssignments/reference/GeocodeMembers.md),
and
[`AssignToDistricts()`](https://aberuiz.github.io/DistrictAssignments/reference/AssignToDistricts.md).
For the "geocode once, apply to many districting files" workflow, call
[`GeocodeMembers()`](https://aberuiz.github.io/DistrictAssignments/reference/GeocodeMembers.md)
once and then
[`AssignToDistricts()`](https://aberuiz.github.io/DistrictAssignments/reference/AssignToDistricts.md)
repeatedly instead.

## Usage

``` r
AssignDistricts(
  memberList,
  StreetCol = "Street.Address",
  CityCol = "City",
  districtsList,
  removeGEO = TRUE,
  districtNames = NULL,
  restrictToDistrictArea = FALSE,
  boundaries = NULL,
  join_type = c("intersects", "within"),
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

- districtsList:

  A single sf layer or file path, a character vector of file paths, or
  an (optionally named) list of sf layers and/or file paths (`.shp` /
  `.geojson` / `.gpkg`). sf objects pass through; paths are read from
  disk.

- removeGEO:

  Drop the geometry column from the result (`TRUE` = clean CSV export).

- districtNames:

  Optional character vector of names, one per layer, used to prefix each
  layer's columns.

- restrictToDistrictArea:

  If `TRUE`, geocoding is limited to the (padded) union extent of the
  district layers. Off by default – see
  [`compute_search_extent()`](https://aberuiz.github.io/DistrictAssignments/reference/compute_search_extent.md)
  for why. Ignored if `boundaries` is supplied.

- boundaries:

  Optional explicit geocoding search extent (a bbox). Overrides
  `restrictToDistrictArea`. Default `NULL` = no restriction.

- join_type:

  How a point must relate to a polygon to be assigned. `"intersects"`
  (default) also matches points lying exactly ON a district boundary (a
  boundary point touching two districts resolves to the first, with a
  message); `"within"` requires the point to be strictly inside, so
  boundary points receive `NA`.

- verbose:

  Print progress and a summary to the console.

## Value

See
[`AssignToDistricts()`](https://aberuiz.github.io/DistrictAssignments/reference/AssignToDistricts.md).

## Examples

``` r
if (FALSE) { # \dontrun{
result <- AssignDistricts(
  memberList    = "members.csv",
  StreetCol     = "Street.Address",
  CityCol       = "City",
  districtsList = list(
    Congressional = "shapes/congress.shp",
    StateSenate   = "shapes/senate.geojson"
  )
)
} # }
```
