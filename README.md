# District Assignments

This project converts addresses to coordinates (geocoding) and then assigns each
address to the district(s) it falls within. It works with **any number of
district layers** in a single run, so you can geocode a member list once and
attach several districting files (congressional, state house, county, etc.) at
the same time.

There are two ways to use it, both built on the same shared core
(`districting_core.R`):

- **Shiny app** (`DistrictingApp.R`) — a point-and-click interface.
- **R script** (`AssignDistricts.R`) — call the functions directly in R.

## Shiny App

Start the app from the repository root (so it can find `districting_core.R`):

```r
shiny::runApp("DistrictingApp.R")
```

![Shiny-Screenshot](https://github.com/aberuiz/DistrictAssignments/blob/main/assets/App_2.png)

1.  **Upload a member list** (`.csv`) with the addresses you want to assign.

2.  **Select the Address and City columns.** By default the app looks for columns
    named `Street.Address` and `City`; if they aren't found you can pick which
    columns to use for geocoding.

3.  **Geocode Addresses.** Press this to geocode the member list. Geocoding is
    independent of the district files, so you can run it as soon as your columns
    are chosen — while you gather district files. A status line reports how many
    addresses matched, and the result is cached so it isn't repeated
    unnecessarily. (If you skip this step, geocoding runs automatically when you
    assign districts.)

4.  **Add district files.** Upload district geometry as `.zip` files, each
    containing a `.shp` or `.geojson`. The button adds files to a list, so you
    can keep adding as many as you like. Give each one a name — the name becomes
    the prefix on that layer's output columns (e.g. a layer named
    `Congressional` turns a `DISTRICT` column into `Congressional_DISTRICT`),
    which keeps columns from different layers distinct.

5.  **Assign Districts.** Assigns each geocoded address to the district(s) it
    falls within, across all uploaded layers. This reuses the cached geocoding,
    so it only runs the (fast) spatial joins.

6.  **Review the results.** A preview table appears. Rows that couldn't be
    geocoded are **highlighted** and carry a `geocode_status` of
    `Missing address` (blank/NA address) or `No geocode match` (address not
    found). Nothing is dropped — every input row appears in the output.

7.  **Download.** Select the columns you want and export to `.csv`.

Additional options:

-   **Remove Geometry Column** (default on) drops the spatial geometry from the
    output for a clean CSV export.
-   **Restrict geocoding to district area** (default off) limits geocoding to the
    area your district files cover. Leave it off for normal use; turn it on only
    if you want to exclude matches far outside your region.
-   **Clear geocode cache** forces the next run to geocode from scratch.
-   **Clear district files** empties the district list.

## R Script

Load the functions and call them directly. Run with the working directory set to
this repository so `districting_core.R` can be sourced.

```r
source("AssignDistricts.R")

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

Every result includes a `geocode_status` column and carries summary statistics
as an attribute; `print_summary(result)` writes them to the console.

## How results are handled

-   **Every input row is kept.** Rows that can't be geocoded are flagged, not
    removed, via `geocode_status` (`OK` / `Missing address` / `No geocode match`).
-   **Rows stay matched.** Geocoding results are aligned to their exact source
    row, so a blank or unmatched address never shifts the other rows.
-   **Overlapping polygons** are resolved to the first matching district per
    layer (with a note), so a point is never duplicated into multiple rows.

## Notes & limitations

-   District geometry must be a `.shp` or `.geojson`. The Shiny app accepts these
    inside a `.zip`.
-   Geocoding uses the R package `arcgisgeocode`. Only the street address and city
    are sent to the geocoding service; all other columns are processed locally.
-   By default geocoding is not restricted to any area. Enabling the restriction
    uses the (padded) combined extent of all uploaded district layers.
