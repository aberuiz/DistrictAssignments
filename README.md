# District Assignments

This project converts addresses to coordinates then finds corresponding district.

You can assign addresses up to two districts.


## Shiny App

The shiny app can be started locally in R with: `shiny::runApp("DistrictingApp.R")`

https://github.com/aberuiz/DistrictAssignments/blob/main/assets/App_2.png

1.  Upload a .csv file with the locations you would like to attach district information

2.  Select Address and City Columns. By default the app will look for columns named 'Street.Address' & 'City'. If not found, a user can select the Address and City columns to be used for geocoding.

3.  Upload district geometry in a .zip format. The .zip file must contain a .shp or .geojson file.

4.  (Optional) Upload a second district geometry. Same requirements as the first geometry.

5.  Remove Geometry Column. By default this is set to 'TRUE' and will remove the geometry columns from your csv to improve csv export

6.  Assign Districts. This will run the addresses and then attach the corresponding district(s).

7.  Select Columns for Export. A preview table will appear and allow you to review the returned data.

8.  Download. Export all selected columns for export into .csv format


## R Script

You can also run a modified version directly in R with the function `AssignDistricts.R`


### Limitations

Currently there are a few known limitations:

-   District geometry's must be in a .shp or .geojson format

-   The shiny app version can only take geometry files in a .zip


### Notes

-   Geocoding is done with the `arcgisgeocoder`. Street Addresses and City is sent, all other information is processed locally.

-   To improve geocoding, the bounding box is set using the first uploaded district geometry
