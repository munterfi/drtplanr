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
#                  system.file("Jegenstorf_i0.RData", package="drtplanr")
#                )
# R             :4.0.0
# =============================================================================

library(drtplanr)

# Example data
aoi <-
  sf::st_read(system.file("example.gpkg", package="drtplanr"), layer = "aoi")

pop <-
  sf::st_read(system.file("example.gpkg", package="drtplanr"), layer = "pop")

# Create model
m <- drt_drtm(
  model_name = "Jegenstorf",
  aoi = aoi, pop = pop,
  n_vir = 10, m_seg = 100
)
m

# Export
drt_export(m, paste0(getwd(), "/inst"))
