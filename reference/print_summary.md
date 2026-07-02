# Print the summary statistics of an assignment result

Prints the summary attached to an
[`AssignToDistricts()`](https://aberuiz.github.io/DistrictAssignments/reference/AssignToDistricts.md)
/
[`AssignDistricts()`](https://aberuiz.github.io/DistrictAssignments/reference/AssignDistricts.md)
result to the console: total, invalid, failed, and successful address
counts, plus the specific rows that failed to geocode.

## Usage

``` r
print_summary(result)
```

## Arguments

- result:

  The return value of
  [`AssignToDistricts()`](https://aberuiz.github.io/DistrictAssignments/reference/AssignToDistricts.md)
  or
  [`AssignDistricts()`](https://aberuiz.github.io/DistrictAssignments/reference/AssignDistricts.md).

## Value

`result`, invisibly.
