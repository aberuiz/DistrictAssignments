# District Assignments

This project converts addresses to coordinates then finds corresponding district.

You can assign addresses up to two districts.


## Shiny

To use the shiny app you can use `shiny::runApp("DistrictingApp.R")`


## R Script

You can also run a modified version directly in R with the function `AssignDistricts.R`


### Limitations

Currently there are a few known limitations:

-   To geocode addresses you must input a .csv file that has columns titled 'Street.Address' & 'City'

-   District geometry's must be in a .shp or .geojson format

-   The shiny app version can only take geometry files in a .zip
