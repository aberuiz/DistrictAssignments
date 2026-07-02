# Assign geocoded points to district layers

Assign already-geocoded points (the output of
[`GeocodeMembers()`](https://aberuiz.github.io/DistrictAssignments/reference/GeocodeMembers.md))
to one or more PREPARED district layers (the output of
[`prepare_district_layers()`](https://aberuiz.github.io/DistrictAssignments/reference/prepare_district_layers.md)).
This function does not prepare/prefix layers itself, so callers reusing
geocoded points across many layer sets pay preparation only once.

## Usage

``` r
AssignToDistricts(
  geocoded,
  layers,
  removeGEO = TRUE,
  join_type = c("intersects", "within"),
  verbose = TRUE
)
```

## Arguments

- geocoded:

  Output of
  [`GeocodeMembers()`](https://aberuiz.github.io/DistrictAssignments/reference/GeocodeMembers.md)
  (all rows, with `geo_x`/`geo_y` and `geocode_status`).

- layers:

  A list of prepared sf layers (see
  [`prepare_district_layers()`](https://aberuiz.github.io/DistrictAssignments/reference/prepare_district_layers.md)).

- removeGEO:

  Drop the geometry column from the result (`TRUE` = clean CSV export).

- join_type:

  How a point must relate to a polygon to be assigned. `"intersects"`
  (default) also matches points lying exactly ON a district boundary (a
  boundary point touching two districts resolves to the first, with a
  message); `"within"` requires the point to be strictly inside, so
  boundary points receive `NA`.

- verbose:

  Print messages about multiply-matched points.

## Value

A data.frame (or sf object if `removeGEO = FALSE`) with one row per
input member, in the original order, with the district layers' prefixed
columns joined on. Summary statistics are attached as the
`"summary_stats"` attribute; see
[`print_summary()`](https://aberuiz.github.io/DistrictAssignments/reference/print_summary.md).

## Details

Every input row is carried through: rows without coordinates become
empty points and simply receive `NA` district columns, and a geocoded
point that falls outside every polygon of a layer receives `NA` for that
layer. Supports any number of layers; overlapping polygons within a
layer resolve to the first match (with a message) so a point is never
duplicated into multiple rows. If an input column has the same name as a
layer's prefixed output column (e.g. a re-uploaded previous export still
carrying `Congressional_DISTRICT`), the input copy is renamed with an
`_input` suffix (with a message) so the fresh assignment keeps the
expected name.

## Examples

``` r
if (FALSE) { # \dontrun{
pts    <- GeocodeMembers("members.csv", "Street.Address", "City")
layers <- prepare_district_layers("shapes/congress.shp", "Congressional")
result <- AssignToDistricts(pts, layers)
} # }
```
