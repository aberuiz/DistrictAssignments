# DistrictAssignments

An R package that converts addresses to coordinates (geocoding) and then
assigns each address to the district(s) it falls within. It works with **any
number of district layers** in a single run, so you can geocode a member list
once and attach several districting files (congressional, state house, county,
etc.) at the same time.

There are two ways to use it, both built on the same functions:

- **Shiny app** — `run_app()`, a point-and-click interface.
- **R functions** — call `AssignDistricts()` and friends directly.

## Installation

```r
# install.packages("pak")
pak::pak("aberuiz/DistrictAssignments")
```

Or from a local checkout:

```r
pak::local_install(".")   # or devtools::install()
```

## Shiny App

```r
library(DistrictAssignments)
run_app()
```

![Shiny-Screenshot](https://github.com/aberuiz/DistrictAssignments/blob/main/assets/App_2.png)

The sidebar is organized into sections — **Member data**, **District files**,
**Options**, and **Download** — matching the steps below.

1.  **Upload a member list** (`.csv`, `.xlsx`, or `.xls`) with the addresses
    you want to assign. (Excel files require the suggested `readxl` package.)

2.  **Select the Address and City columns.** By default the app looks for columns
    named `Street.Address` and `City`; if they aren't found you can pick which
    columns to use for geocoding.

3.  **Geocode Addresses.** Press this to geocode the member list. Geocoding is
    independent of the district files, so you can run it as soon as your columns
    are chosen — while you gather district files. A status line reports how many
    addresses matched, and the result is cached so it isn't repeated
    unnecessarily. (If you skip this step, geocoding runs automatically when you
    assign districts.)

4.  **Add district files.** Upload district geometry as `.gpkg` or `.geojson`
    files directly, or as `.zip` archives (required for `.shp`, which needs its
    sidecar files). The button adds files to a list, so you
    can keep adding as many as you like; each file can be removed individually
    with the ✕ on its card. Give each one a name — the name becomes
    the prefix on that layer's output columns (e.g. a layer named
    `Congressional` turns a `DISTRICT` column into `Congressional_DISTRICT`),
    which keeps columns from different layers distinct.

5.  **Assign Districts.** Assigns each geocoded address to the district(s) it
    falls within, across all uploaded layers. This reuses the cached geocoding,
    so it only runs the (fast) spatial joins.

6.  **Review the results.** A summary of the run appears, and the results
    show in a table with per-column filters and a **Show/hide columns**
    button for taming wide results (long cell text is truncated — hover to
    see the full value). Rows that couldn't be
    geocoded are **highlighted red** and carry a `geocode_status` of
    `Missing address` (blank/NA address), `No geocode match` (address not
    found), or `Geocoder error` (the geocoding request failed — e.g. a
    network hiccup — so clearing the geocode cache and re-running may fix
    it). Geocodes with a low match score (`geo_score` below 85) are
    **highlighted amber** — they matched, but possibly to the wrong place, so
    give them a second look. Nothing is dropped — every input row appears in
    the output.

    The **Map tab** (requires the suggested `leaflet` package) shows the same
    results on a map: district polygons labeled with per-district member
    counts, points colored by the assignment of the layer you pick, gray
    points for addresses outside every district, and a count table underneath.

7.  **Download.** The Download section opens once results exist, with your
    original columns and the district assignments pre-selected (the geocoder
    bookkeeping columns — `original_row_id`, `geocode_status`, `geo_*` — are
    offered but unchecked). Adjust the selection if you like ("Select all" /
    "Select none" links included) and export to `.csv`.

Additional options:

-   **Retry failed geocodes with the US Census geocoder** (default on) gives
    addresses the ArcGIS geocoder can't find a second chance against the free
    US Census Bureau geocoder (US addresses only). Recovered rows are marked
    `geo_source = "Census"`.
-   **Remove Geometry Column** (default on) drops the spatial geometry from the
    output for a clean CSV export.
-   **Restrict geocoding to district area** (default off) limits geocoding to the
    area your district files cover. Leave it off for normal use; turn it on only
    if you want to exclude matches far outside your region.
-   **Clear geocode cache** forces the next run to geocode from scratch.
-   **Clear district files** empties the district list.
-   `run_app(max_upload_mb = 50)` raises the 20 MB upload limit.

## R Functions

```r
library(DistrictAssignments)

# One-shot: geocode and assign in a single call. Use a NAMED list to control the
# output column prefixes.
result <- AssignDistricts(
  memberList    = "members.csv",
  StreetCol     = "Street.Address",
  CityCol       = "City",
  districtsList = list(
    Congressional = "shapes/congress.shp",
    StateSenate   = "shapes/senate.geojson"
  )
)
# -> columns include Congressional_DISTRICT, StateSenate_DISTRICT, and a
#    geocode_status column flagging any "Missing address" / "No geocode match".
```

To geocode once and apply **many** districting files efficiently, split the two
steps — the addresses are geocoded a single time and reused:

```r
members  <- read.csv("members.csv")
layers_a <- prepare_district_layers("shapes/congress.shp",  "Congressional")
layers_b <- prepare_district_layers("shapes/senate.geojson", "StateSenate")

pts <- GeocodeMembers(members, "Street.Address", "City")  # geocode once

result_a <- AssignToDistricts(pts, layers_a)
result_b <- AssignToDistricts(pts, layers_b)
```

By default geocoding is unrestricted (depends only on address + city). To limit
it to the district area, pass `restrictToDistrictArea = TRUE` to
`AssignDistricts()`, or build an extent yourself with
`compute_search_extent()` and pass it to `GeocodeMembers()`.

Every result includes a `geocode_status` column and carries summary statistics
as an attribute; `print_summary(result)` writes them to the console.

## How results are handled

-   **Every input row is kept.** Rows that can't be geocoded are flagged, not
    removed, via `geocode_status` (`OK` / `Missing address` /
    `No geocode match` / `Geocoder error`).
-   **Rows stay matched.** Geocoding results are aligned to their exact source
    row, so a blank or unmatched address never shifts the other rows.
-   **Points outside every district stay blank.** A geocoded address that
    doesn't fall inside any polygon of a layer simply gets `NA` for that
    layer's columns (and plots gray on the app's map).
-   **Large lists are geocoded in chunks** (500 addresses per request batch),
    so a transient service failure only affects its own chunk: those rows are
    flagged `Geocoder error` (retryable — and the census fallback still gets
    a chance at them) instead of the whole run aborting.
-   **Layer names are made unique.** Any number of district layers can be
    used at once; duplicate names (e.g. two files both named `congress`) are
    disambiguated (`congress`, `congress_1`) so every layer keeps distinct,
    predictable output columns. If an input column already has the same name
    as a layer's output column (say, a re-uploaded export carrying
    `Congressional_DISTRICT`), the input copy is renamed with an `_input`
    suffix and the fresh assignment keeps the expected name.
-   **Overlapping polygons** are resolved to the first matching district per
    layer (with a note), so a point is never duplicated into multiple rows.
-   **Boundary points are assigned.** By default a point lying exactly on a
    district boundary matches (`join_type = "intersects"`); a point touching
    two districts resolves to the first, with a note. Pass
    `join_type = "within"` to require points to be strictly inside a district
    (boundary points then get `NA`).
-   **Reserved columns.** `original_row_id`, `geocode_status`, and all `geo_*`
    columns are rewritten by `GeocodeMembers()`. If your input CSV already has
    them (e.g. you re-upload a previous export), they are replaced with fresh
    values so stale results can't leak into a new run.

## Notes & limitations

-   District geometry must be a `.shp`, `.geojson`, or `.gpkg` (GeoPackage),
    and must carry a coordinate reference system (for shapefiles, keep the
    `.prj` sidecar). A multi-layer GeoPackage uses its first layer, with a
    note.
-   Geocoding uses the R package
    [`arcgisgeocode`](https://github.com/R-ArcGIS/arcgisgeocode) (ArcGIS World
    Geocoder), optionally falling back to the US Census Bureau geocoder for
    failed addresses (`censusFallback = TRUE`; US only). Only the street
    address and city are sent to the geocoding service(s); all other columns
    are processed locally.
-   By default geocoding is not restricted to any area. Enabling the restriction
    uses the (padded) combined extent of all uploaded district layers.
-   Spatial joins are planar (s2 disabled), which is the appropriate model for
    administrative district polygons.

## Development

```r
devtools::load_all()   # load the package
devtools::test()       # run the test suite (offline; one live geocode test
                       # runs only when NOT_CRAN=true and a connection exists)
devtools::check()      # full R CMD check
```
