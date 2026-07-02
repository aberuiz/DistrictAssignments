# Changelog

## DistrictAssignments 0.1.5

### New features

- Census geocoder fallback:
  [`GeocodeMembers()`](https://aberuiz.github.io/DistrictAssignments/reference/GeocodeMembers.md)
  and
  [`AssignDistricts()`](https://aberuiz.github.io/DistrictAssignments/reference/AssignDistricts.md)
  gain a `censusFallback` argument (default `FALSE`; the app’s checkbox
  defaults to on). Addresses the ArcGIS geocoder fails on are retried
  against the free US Census Bureau geocoder; recovered rows get
  coordinates, status `"OK"`, and `geo_source = "Census"`. A new
  `geo_source` column records which service located each row.
- Results map: the app’s main panel is now a Table/Map tabset. The map
  (built with the suggested `leaflet` package) shows every district
  layer’s polygons as toggleable groups labeled with per-district member
  counts, and plots the geocoded points colored by the assignment of a
  selectable layer. Points outside every district plot gray; rows
  without coordinates are counted in a caption. A per-district member
  count table appears under the map.
- [`prepare_district_layers()`](https://aberuiz.github.io/DistrictAssignments/reference/prepare_district_layers.md)
  now names its returned list with the resolved layer names.
- Assigned results now retain the `geo_x`/`geo_y` coordinate columns
  (previously consumed by the spatial join), so exports include
  coordinates and the map can plot them.

## DistrictAssignments 0.1.0

First release as an R package (previously a set of standalone scripts).

### New features

- [`run_app()`](https://aberuiz.github.io/DistrictAssignments/reference/run_app.md)
  launches the Shiny interface; the command-line workflow is the
  exported functions
  [`AssignDistricts()`](https://aberuiz.github.io/DistrictAssignments/reference/AssignDistricts.md),
  [`GeocodeMembers()`](https://aberuiz.github.io/DistrictAssignments/reference/GeocodeMembers.md),
  [`prepare_district_layers()`](https://aberuiz.github.io/DistrictAssignments/reference/prepare_district_layers.md),
  [`AssignToDistricts()`](https://aberuiz.github.io/DistrictAssignments/reference/AssignToDistricts.md),
  [`compute_search_extent()`](https://aberuiz.github.io/DistrictAssignments/reference/compute_search_extent.md),
  and
  [`print_summary()`](https://aberuiz.github.io/DistrictAssignments/reference/print_summary.md).
- Points lying exactly on a district boundary are now assigned
  (`join_type = "intersects"`, the default); pass `join_type = "within"`
  for the previous strict behavior.
- Member lists can be Excel files (`.xlsx` / `.xls`) as well as CSV, in
  both the app and
  [`GeocodeMembers()`](https://aberuiz.github.io/DistrictAssignments/reference/GeocodeMembers.md)
  (requires the suggested `readxl` package).
- District layers can be GeoPackage files (`.gpkg`) as well as `.shp`
  and `.geojson`; the app accepts `.gpkg` and `.geojson` uploads
  directly, without zipping.
- The app highlights low-confidence geocodes (`geo_score` below 85) in
  amber in the results preview.

### Bug fixes

- Re-uploading a previous export no longer lets stale `geo_x`/`geo_y`
  coordinates survive on rows that fail geocoding: `original_row_id`,
  `geocode_status`, and all `geo_*` columns are reserved and rewritten
  on every run.
- The geocoder’s nested `extents` field is no longer carried into
  results, where it broke CSV export.
- District name inputs in the app are keyed by upload, so clearing the
  district list and adding new files no longer inherits names typed for
  earlier files.
- A character vector of layer paths, a layer with no CRS, and `NA`
  district names now work or fail with clear messages instead of
  confusing errors.
- Spatial joins are always planar (s2 disabled, caller’s setting
  restored), so results no longer depend on which file formats were read
  earlier in the session.
