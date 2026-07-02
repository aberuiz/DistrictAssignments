# Package index

## One-shot workflow

Geocode a member list and assign districts in a single call.

- [`AssignDistricts()`](https://aberuiz.github.io/DistrictAssignments/reference/AssignDistricts.md)
  : Geocode a member list and assign it to districts in one call

## Geocode once, assign many

The efficient path when the same addresses are joined against several
district sets: geocode a single time, then reuse the points.

- [`GeocodeMembers()`](https://aberuiz.github.io/DistrictAssignments/reference/GeocodeMembers.md)
  : Geocode a member list
- [`prepare_district_layers()`](https://aberuiz.github.io/DistrictAssignments/reference/prepare_district_layers.md)
  : Prepare district layers for assignment
- [`AssignToDistricts()`](https://aberuiz.github.io/DistrictAssignments/reference/AssignToDistricts.md)
  : Assign geocoded points to district layers
- [`compute_search_extent()`](https://aberuiz.github.io/DistrictAssignments/reference/compute_search_extent.md)
  : Compute a geocoding search extent from district layers

## Results

- [`print_summary()`](https://aberuiz.github.io/DistrictAssignments/reference/print_summary.md)
  : Print the summary statistics of an assignment result

## Shiny app

- [`run_app()`](https://aberuiz.github.io/DistrictAssignments/reference/run_app.md)
  : Launch the district-assignment Shiny app
