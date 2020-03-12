# ------------------------------------------------------------------------------
# drtplanr: Minimize the global energy of the model in a model run
#
# File: ~/drtplanr/02_model_run.R
# Author: Merlin Unterfinger (info@munterfinger.ch)
# Date: 2020-03-07
#
# GNU General Public License v3.0
# ------------------------------------------------------------------------------

## Load drtplanr
source("R/drtplanr.R")

## Read data
aoi <- st_read("data/prep.gpkg", layer = "aoi", quiet	= TRUE)
pop <- st_read("data/prep.gpkg", layer = "pop", quiet	= TRUE)


# Create model
m <- drt_drtm(
  model_name = "Jegenstorf",
  aoi = aoi, pop = pop,
  n_vir = 7, m_seg = 100
)
m

# Export model
drt_export(m, path = "results")

# Import model
m <- drt_import("results/Jegenstorf_i0.RData")

# Minimze the energy of the model
m1 <- drt_iterate(m, 100)
drt_export(m1, path = "results")
drt_save_graphics(m1, path = paste0(getwd(), "/docs"))
m1

# again
m2 <- drt_iterate(m1, 900)
drt_export(m2, path = "results")
drt_save_graphics(m2, path = paste0(getwd(), "/docs"))
m2

# and again
m3 <- drt_iterate(m2, 4000)
drt_export(m3, path = "results")
drt_save_graphics(m3, path = paste0(getwd(), "/docs"))
m3
