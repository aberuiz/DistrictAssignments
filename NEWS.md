# DistrictAssignments 0.1.0

First release as an R package (previously a set of standalone scripts).

## New features

* `run_app()` launches the Shiny interface; the command-line workflow is the
  exported functions `AssignDistricts()`, `GeocodeMembers()`,
  `prepare_district_layers()`, `AssignToDistricts()`,
  `compute_search_extent()`, and `print_summary()`.
* Points lying exactly on a district boundary are now assigned
  (`join_type = "intersects"`, the default); pass `join_type = "within"` for
  the previous strict behavior.
* Member lists can be Excel files (`.xlsx` / `.xls`) as well as CSV, in both
  the app and `GeocodeMembers()` (requires the suggested `readxl` package).
* District layers can be GeoPackage files (`.gpkg`) as well as `.shp` and
  `.geojson`; the app accepts `.gpkg` and `.geojson` uploads directly, without
  zipping.
* The app highlights low-confidence geocodes (`geo_score` below 85) in amber
  in the results preview.

## Bug fixes

* Re-uploading a previous export no longer lets stale `geo_x`/`geo_y`
  coordinates survive on rows that fail geocoding: `original_row_id`,
  `geocode_status`, and all `geo_*` columns are reserved and rewritten on
  every run.
* The geocoder's nested `extents` field is no longer carried into results,
  where it broke CSV export.
* District name inputs in the app are keyed by upload, so clearing the
  district list and adding new files no longer inherits names typed for
  earlier files.
* A character vector of layer paths, a layer with no CRS, and `NA` district
  names now work or fail with clear messages instead of confusing errors.
* Spatial joins are always planar (s2 disabled, caller's setting restored),
  so results no longer depend on which file formats were read earlier in the
  session.
