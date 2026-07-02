# Launch the district-assignment Shiny app

A point-and-click interface over
[`GeocodeMembers()`](https://aberuiz.github.io/DistrictAssignments/reference/GeocodeMembers.md)
and
[`AssignToDistricts()`](https://aberuiz.github.io/DistrictAssignments/reference/AssignToDistricts.md):
upload a member CSV, geocode it, add any number of district files
(`.gpkg`, `.geojson`, or zipped `.shp`), review the results as a table
or on a map, and download the assigned results. Requires the suggested
packages `shiny`, `DT`, and `bslib`; the map tab additionally uses
`leaflet` (a hint is shown if it isn't installed).

## Usage

``` r
run_app(max_upload_mb = 20, ...)
```

## Arguments

- max_upload_mb:

  Maximum upload size in megabytes (default 20).

- ...:

  Passed on to
  [`shiny::runApp()`](https://rdrr.io/pkg/shiny/man/runApp.html) (e.g.
  `port`, `launch.browser`).

## Value

Runs the app; does not return while the app is live.

## Examples

``` r
if (FALSE) { # \dontrun{
run_app()
} # }
```
