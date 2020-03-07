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
read_config()


## Read data
tmessage("Read pre processed data sets from '/prep'")
sta <- st_read("prep/station.gpkg", quiet	= TRUE)
iso <- st_read("prep/isoline.gpkg", quiet	= TRUE)
tra <- st_read("prep/traffic.gpkg", quiet	= TRUE)
roa <- st_read("prep/road.gpkg", quiet = TRUE)
seg <- st_read("prep/segment.gpkg", quiet	= TRUE)
pop <- st_read("prep/statpop.gpkg", quiet	= TRUE)


## Params
n_sta <- 10            # Number of stations to plan
n_seg <- nrow(seg)     # Number of "possible" stations
n_sim <- 1000          # Number of iterations
calc_energy <-         # Set energy calculation function:
  e_walkDrive_pop      # 'e_walk_pop()' or 'e_walkDrive_pop()'


## Initalize model
tmessage("Initalize model")

# Existing stations
idx_const <- st_nearest_feature(sta, seg)

# Random sample
idx <- c(sample_exclude(1:n_seg, n_sta, idx_const), idx_const)
idx_start <- idx

# Energy table
energy <- data.table(
  iteration = seq(0, n_sim),
  value = 0
)
energy[1, ]$value <- sum(calc_energy(idx, seg, pop))


## Iterate
tmessage("Minimizing the global energy of the model")
for (i in 2:(n_sim + 1)) {
  e_old <- energy[i - 1, ]$value
  idx_new_pos <- sample(1:n_sta, 1)
  idx_old <- idx[idx_new_pos]
  idx_new <- sample_exclude(1:n_seg, 1, idx)
  idx[idx_new_pos] <- idx_new
  e_new <- sum(calc_energy(idx, seg, pop))
  cat(sprintf("\r  Iteration: %s, e0: %s, e1: %s \r",
                  i - 1, round(e_old, 1), round(e_new, 1)))
  if (e_old > e_new) {
    energy[i, ]$value <- e_new
  } else {
    energy[i, ]$value <- e_old
    idx[idx_new_pos] <- idx_old
  }
}
tmessage("Model run completed (iterations: %s, e0: %s, e1: %s)" %>%
    sprintf(n_sim, energy[1, ]$value %>% round(1), energy[n_sim + 1, ]$value %>% round(1)))


## Save result
tmessage("Save model results")
results <- list(sta_const = seg[idx_const, ],
                sta_init = seg[idx_start[1:n_sta], ],
                sta_final = seg[idx[1:n_sta], ],
                energy = energy)
save(results, file = "result/model_results.RData")
