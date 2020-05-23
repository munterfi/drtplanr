#!/usr/bin/env Rscript
# -----------------------------------------------------------------------------
# Name          :02_create_examples.R
# Description   :Create Area of Intereset (aoi) and population (pop) data set
#                for the example data included in the drtplanr package. The
#                datasets are stored it to 'inst/example.gpkg' in the package
#                root.
# Author        :Merlin Unterfinger <info@munterfinger.ch>
# Date          :2020-05-23
# Version       :0.1.0
# Usage         :./02_create_examples.R
# Notes         :- Read the 'aoi' layer from the package example data via:
#                aoi <-
#                  "example.gpkg" %>% system.file(package="drtplanr") %>%
#                  sf::st_read(layer = "aoi", quiet = TRUE)
#                - Read the 'pop' layer from the package example data via:
#                pop <-
#                  "example.gpkg" %>% system.file(package="drtplanr") %>%
#                  sf::st_read(layer = "pop", quiet = TRUE)
# R             :4.0.0
# =============================================================================

# Get args from commandline
args <- commandArgs(trailingOnly = TRUE)

# test if there is at least one argument: if not, return an error
if (length(args)==0) {
  stop("At least one argument must be supplied (HERE API key).n", call. = FALSE)
}

library(data.table)
library(hereR)
library(sf)

# Setup hereR
set_verbose(TRUE)
set_key(args[1])


## Data set: 'aoi'
# Geocode address
geo <-
  "Jegenstorf Bahnhof RBS, Bern, Schweiz" %>%
  geocode()

# Get stations near geocoded address
sta <-
  geo %>%
  station(radius = 2000, results = 50)

# Create isochrone around the station by a 3 1/4 min car drive
aoi <-
  sta[2, ] %>%
  isoline(mode = "car", range = 3.25*60, traffic = FALSE)
aoi$id <- 1

# Save to GeoPackage
aoi %>%
  st_write("inst/example.gpkg", delete_layer = TRUE, layer = "aoi")


## Data set: 'pop'
# Read datasets
statent <-
  fread("data-raw/statent/STATENT2017_N08_V190822.csv")
statpop <-
  fread("data-raw/statpop/STATPOP2018.csv")

# Move centroids to center of the hectares
statent[, c("id", "x", "y", "statent") := .(
  paste0(E_KOORD, "_", N_KOORD), E_KOORD + 50, N_KOORD + 50, B1708T)]
statpop[, c("id", "x", "y", "statpop") := .(
  paste0(E_KOORD, "_", N_KOORD), E_KOORD + 50, N_KOORD + 50, B18BTOT)]

# Merge datasets and replace NAs
pop <-
  statent %>%
  merge(statpop, by = "id", all = TRUE) %>%
  .[, .(id, statent, statpop)] %>%
  .[, lapply(.SD, function(x) {ifelse(is.na(x), 0, x)})]

# Retrieve coords
pop[, c("x", "y") := tstrsplit(`id`, "_", fixed = TRUE)]

# Overwrite ID and remove NAs
pop[, c("id", "n") := .(seq(1, nrow(pop)), statent + statpop)]

# Create sf object and intersect with AOI
pop <-
  pop %>%
  st_as_sf(coords = c("x", "y")) %>%
  st_set_crs(2056) %>%
  st_intersection(aoi %>% st_transform(2056) %>% st_geometry()) %>%
  st_transform(4326)

# Save to GeoPackage
pop %>%
  st_write("inst/example.gpkg", delete_layer = TRUE, layer = "pop")
