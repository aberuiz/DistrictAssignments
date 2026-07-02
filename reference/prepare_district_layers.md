# Prepare district layers for assignment

Normalize, read, validate and name-prefix one or more district layers so
they are ready to hand to
[`AssignToDistricts()`](https://aberuiz.github.io/DistrictAssignments/reference/AssignToDistricts.md).

## Usage

``` r
prepare_district_layers(districtsList, districtNames = NULL)
```

## Arguments

- districtsList:

  A single sf layer or file path, a character vector of file paths, or
  an (optionally named) list of sf layers and/or file paths (`.shp` /
  `.geojson` / `.gpkg`). sf objects pass through; paths are read from
  disk.

- districtNames:

  Optional character vector of names, one per layer, used to prefix each
  layer's columns.

## Value

A list of prepared sf layers with validated geometry, a coordinate
reference system, and name-prefixed attribute columns.

## Details

Each layer's attribute columns are prefixed with the layer's name (e.g.
a `DISTRICT` column in a layer named `Congressional` becomes
`Congressional_DISTRICT`) so columns from different layers stay distinct
in the joined output. Names are resolved per layer, in order of
precedence: an explicit `districtNames` entry, the name on the
`districtsList` element, the file's base name (for path elements), then
a positional fallback.

## Examples

``` r
if (FALSE) { # \dontrun{
layers <- prepare_district_layers(list(
  Congressional = "shapes/congress.shp",
  StateSenate   = "shapes/senate.geojson"
))
} # }
```
