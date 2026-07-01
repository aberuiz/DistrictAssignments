# =============================================================================
# AssignDistricts.R -- command-line entry point
#
# Loads the required packages and the shared core logic, then exposes the
# district-assignment functions. The actual implementation lives in
# districting_core.R so the script and the Shiny app share one code path.
#
# Run with the working directory set to this repository so the source() below
# resolves (e.g. open the project, or setwd() to the repo root first).
# =============================================================================

#install.packages(c("sf","arcgisgeocode","yyjsonr","dplyr","gdalraster"))
library(sf)
library(arcgisgeocode)
library(yyjsonr)
library(dplyr)
library(gdalraster)

# Shared core: prepare_district_layers(), compute_search_extent(),
# GeocodeMembers(), AssignToDistricts(), AssignDistricts(), print_summary().
source("districting_core.R")

# ---------------------------------------------------------------------------
# Usage
# ---------------------------------------------------------------------------
#
# One-shot: geocode a member list and assign it to one or more district layers.
# districtsList accepts a single path, a list of paths, or a *named* list whose
# names become the output column prefixes.
#
#   result <- AssignDistricts(
#     memberList    = "members.csv",
#     StreetCol     = "Street.Address",
#     CityCol       = "City",
#     districtsList = list(
#       Congressional = "shapes/congress.shp",
#       StateSenate   = "shapes/senate.geojson"
#     )
#   )
#
#   # -> columns include Congressional_DISTRICT, StateSenate_DISTRICT, and a
#   #    geocode_status column flagging any "Missing address" / "No geocode match".
#
# By default geocoding is unrestricted (depends only on address + city). To limit
# it to the district area, pass restrictToDistrictArea = TRUE.
#
# Geocode once, apply to MANY districting files (the efficient path when you have
# several district sets for the same addresses -- geocoding runs a single time):
#
#   members <- read.csv("members.csv")
#   layers_a <- prepare_district_layers("shapes/congress.shp",  "Congressional")
#   layers_b <- prepare_district_layers("shapes/senate.geojson", "StateSenate")
#
#   pts  <- GeocodeMembers(members, "Street.Address", "City")  # once, unrestricted
#   # ...or restrict to the district area:
#   #   bbox <- compute_search_extent(c(layers_a, layers_b))
#   #   pts  <- GeocodeMembers(members, "Street.Address", "City", bbox)
#
#   result_a <- AssignToDistricts(pts, layers_a)  # reuse pts
#   result_b <- AssignToDistricts(pts, layers_b)  # reuse pts
#
# Each result carries summary statistics as an attribute; print_summary(result)
# writes them to the console.
