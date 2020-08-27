#!/usr/bin/env Rscript
# -----------------------------------------------------------------------------
# Name          :04_drtm_object.R
# Description   :Creates a 'drtm' object and saves them to 'inst'.
# Author        :Merlin Unterfinger <info@munterfinger.ch>
# Date          :2020-05-23
# Version       :0.1.0
# Usage         :./04_drtm_object.R
# Notes         :Load from package examples using:
#                m <- drt_import(
#                  system.file("example.RData", package="drtplanr")
#                )
# R             :4.0.0
# =============================================================================

library(drtplanr)
set.seed(123)

# Example data
poi <-
  sf::st_read(system.file("example.gpkg", package = "drtplanr"), layer = "poi")

aoi <-
  sf::st_read(system.file("example.gpkg", package = "drtplanr"), layer = "aoi")

pop <-
  sf::st_read(system.file("example.gpkg", package = "drtplanr"), layer = "pop")

# Create model
m <- drt_drtm(
  model_name = "example",
  aoi = aoi, poi = poi, pop = pop,
  n_vir = 20, m_seg = 100
)
m

# Iterate
m <- drt_iterate(m, 1000)

# Export
drt_export(m, paste0(getwd(), "/inst"))

# Save graphics
drt_save_graphics(m, paste0(getwd(), "/docs"))
