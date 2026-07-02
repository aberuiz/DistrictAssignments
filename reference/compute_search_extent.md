# Compute a geocoding search extent from district layers

Union bounding box of all layers, reprojected to WGS84 (EPSG:4326),
suitable for passing to the geocoder as a search extent. Using the union
(rather than just one layer) means the box covers the TOTAL area the
districts span.

## Usage

``` r
compute_search_extent(layers, pad = 0.05)
```

## Arguments

- layers:

  A list of prepared sf layers (see
  [`prepare_district_layers()`](https://aberuiz.github.io/DistrictAssignments/reference/prepare_district_layers.md)).

- pad:

  Fraction of the box's width/height added on every side (default 5%).
  Keeps edge addresses from falsely failing to geocode.

## Value

An
[`sf::st_bbox()`](https://r-spatial.github.io/sf/reference/st_bbox.html)
in EPSG:4326.

## Details

Geocoding with a search extent is opt-in (see
[`AssignDistricts()`](https://aberuiz.github.io/DistrictAssignments/reference/AssignDistricts.md)'s
`restrictToDistrictArea`). It is off by default because a search extent
hard-limits candidates to inside the box: a valid address just outside
every district would return no candidate and be mis-flagged as a geocode
failure, when really it's "found, just unassigned".
