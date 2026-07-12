# DistrictAssignments 0.1.9

* District-column guessing now recognizes TIGER congressional codes
  (`CD118FP`-style); previously such files could fall back to OBJECTID.
* The guessing heuristic moved to the package internals and is covered by
  unit tests.
* The map's "using column" picker no longer goes blank after re-running
  Assign Districts, and switching layers no longer redraws the map twice.
* "Remove Geometry Column" works again: unchecking it now adds a
  `geometry_wkt` column (WKT text) to the CSV download. The results table
  never displays it and now says so.
* Internal: results are flattened to a plain data frame once at the source
  instead of every consumer dropping `sf` geometry itself.
* District layers are now geometry-repaired exactly once, on every input path
  — 0.1.8 removed the double repair only for file-path inputs, so app uploads
  still paid it.

# DistrictAssignments 0.1.8

Bug-fix release: clean output when geometry is kept, and a map that
identifies districts correctly on real-world files.

## Bug fixes

* Downloading with "Remove Geometry Column" unchecked no longer writes a
  bogus `geometry` column of stringified points (e.g. `c(-97, 30)`) to the
  CSV, and the results table no longer renders it. The `sf` geometry column
  is sticky under `[` selection, so it is now dropped before columns are
  chosen for both the table and the download.
* The results map no longer identifies districts by a layer's first
  attribute column, which on real district files (e.g. Census TIGER, whose
  leading column is `STATEFP`/`OBJECTID`/`Shape_Area`) collapsed every point
  to one color and tallied members by an irrelevant field. It now picks a
  district-like column (`DISTRICT`, `NAMELSAD`, `CD`, `SLDU`/`SLDL`, `WARD`,
  `PRECINCT`, `GEOID`, ...) by name, and a new "using column" dropdown lets
  you override the guess per selected layer for point coloring and the
  member-count table.

## Other

* `prepare_district_layers()` no longer repairs geometry twice for
  file-path inputs (`read_spatial_file()` already repairs on read); the
  extra `st_make_valid()` pass now runs only on directly-supplied `sf`
  layers that still need it.
* `.DS_Store` is now git-ignored and untracked.

# DistrictAssignments 0.1.7

App UI overhaul: the interface now stays usable with many district files,
many columns, and long district lists.

## App improvements

* The sidebar is reorganized into accordion sections (Member data, District
  files, Options, Download) on a `bslib::page_sidebar()` layout, with the
  primary "Assign Districts" button styled and placed prominently. The
  Download section opens automatically once results exist.
* Lists that grow with the data are capped with their own scroll areas
  instead of stretching the sidebar: the per-file district naming list and
  the download column picker.
* Each uploaded district file now shows as a compact card with the file name
  truncated (full name on hover) and a remove button, so a single bad upload
  no longer requires clearing and re-adding everything.
* The download column picker defaults to the original member columns plus
  the district assignments (geocoder bookkeeping columns --
  `original_row_id`, `geocode_status`, `geo_*` -- are offered but
  unchecked), and gains "Select all" / "Select none" links.
* The results table shows 25 rows, adds a "Show/hide columns" button and
  per-column filters, and truncates long cell text at 40 characters with the
  full value shown on hover.
* The post-run summary appears in a modal dialog instead of a notification;
  the notification collapsed its line breaks into one long paragraph.
* The map's per-district member count table is paged and searchable, so a
  layer with hundreds of districts can't stretch the page; the map's layer
  control collapses when more than 5 layers are loaded.
* The street/city column selectors are searchable and server-side
  (`selectize`, `server = TRUE`), so member files with hundreds of columns
  stay fast.
* Requires `bslib` >= 0.6.1 for the app.

# DistrictAssignments 0.1.6

Production-hardening release: large member lists, many district layers, and
messy input can no longer sink a run.

## Bug fixes and robustness

* Geocoding requests are now sent in chunks of 500 with per-chunk error
  handling. Previously a single failed request (timeout, rate limit, server
  error) aborted the entire geocode call -- on a list of thousands of
  addresses, one transient failure lost everything. Now only the affected
  chunk's rows are left ungeocoded, with a warning naming the row range.
* New `geocode_status` value `"Geocoder error"` marks rows whose geocode
  *request* failed (retryable), distinguishing them from `"No geocode match"`
  (the address itself didn't resolve). They are counted separately in the
  summary, shown in the app's status line, and announced with an app
  notification explaining how to retry; the census fallback still retries
  them.
* Street/city columns are coerced to character before geocoding, so a column
  that Excel or CSV read as numeric (house numbers, ZIP-like values) no
  longer aborts the run with a type error.
* Duplicate district layer names are made unique (with a message). Previously
  two layers with the same name -- e.g. two uploaded files both named
  `congress` -- produced colliding output columns that sf silently renamed to
  `.x`/`.y`, which also broke the app's map coloring and count table. Layer
  names that sanitize to nothing (e.g. `"!!!"`) now fall back to a positional
  `District_N` prefix instead of leaving columns unprefixed.
* Input columns that collide with a layer's output columns (typically a
  re-uploaded previous export still carrying `Congressional_DISTRICT`) are
  renamed with an `_input` suffix, with a message, so the fresh assignment
  always lands under its documented column name.
* District layers with no features are rejected with a clear error (they
  would assign `NA` everywhere and previously made
  `restrictToDistrictArea = TRUE` fail with an invalid extent);
  `compute_search_extent()` also validates that the extent is finite. A layer
  containing no polygons (e.g. a points file uploaded by mistake) triggers a
  warning.
* The census fallback now stops after 5 consecutive request failures instead
  of burning a 30-second timeout on every remaining row when the service is
  unreachable, and prints progress every 100 rows on large retry batches.
* Failed-address listings (console summary and app notification) are capped
  at 10 rows with a "... and N more" note, instead of flooding the console or
  screen when thousands of rows fail.
* A member list where no row geocodes no longer emits confusing
  "no non-missing arguments to max" warnings during assignment.

# DistrictAssignments 0.1.5

## New features

* Census geocoder fallback: `GeocodeMembers()` and `AssignDistricts()` gain a
  `censusFallback` argument (default `FALSE`; the app's checkbox defaults to
  on). Addresses the ArcGIS geocoder fails on are retried against the free US
  Census Bureau geocoder; recovered rows get coordinates, status `"OK"`, and
  `geo_source = "Census"`. A new `geo_source` column records which service
  located each row.
* Results map: the app's main panel is now a Table/Map tabset. The map (built
  with the suggested `leaflet` package) shows every district layer's polygons
  as toggleable groups labeled with per-district member counts, and plots the
  geocoded points colored by the assignment of a selectable layer. Points
  outside every district plot gray; rows without coordinates are counted in a
  caption. A per-district member count table appears under the map.
* `prepare_district_layers()` now names its returned list with the resolved
  layer names.
* Assigned results now retain the `geo_x`/`geo_y` coordinate columns
  (previously consumed by the spatial join), so exports include coordinates
  and the map can plot them.

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
