library(sf)
library(data.table)
library(hereR)
set_key(jsonlite::read_json("config.json")$here$key)

## Read data
message(Sys.time(), " Read pre processed data sets from '/prep'")
sta <- st_read("prep/station.gpkg", quiet	= TRUE)
iso <- st_read("prep/isoline.gpkg", quiet	= TRUE)
tra <- st_read("prep/traffic.gpkg", quiet	= TRUE)
roa <- st_read("prep/road.gpkg", quiet	= TRUE)
seg <- st_read("prep/segment.gpkg", quiet	= TRUE)
pop <- st_read("prep/statpop.gpkg", quiet	= TRUE)

## Functions
message(Sys.time(), " Define energy function")
# Sampling
sample_exclude <- function(x, size, const) {
  sampling <- TRUE
  while (sampling) {
    idx <- sample(x, size, replace = FALSE)
    if (any(idx %in% const)) {
      message("\r  Duplicate stations: Resampling...")
    } else {
      sampling <- FALSE
    }
  }
  return(idx)
}
# Energy
calc_energy <- function(idx, seg, pop) {
  # Route with HERE
  rts <-
    route_matrix(seg[idx, ], pop, mode = "pedestrian") %>%
    as.data.table()
  # Get nearest station
  nn <- rts[, .SD[which.min(travelTime)], by = list(destIndex)]
  nn$pop <- pop[nn$destIndex, ]$n
  return(nn[, sum(travelTime/pop), by = list(origIndex)])
}

calc_energy2 <- function(idx, seg, pop) {
  # Route with HERE
  rts <-
    route_matrix(seg[idx, ], pop, mode = "pedestrian") %>%
    as.data.table()
  # Get nearest station
  nn <- rts[, .SD[which.min(travelTime)], by = list(destIndex)]
  nn$pop <- pop[nn$destIndex, ]$n
  return(nn[, sum(travelTime/pop), by = list(origIndex)])
}
