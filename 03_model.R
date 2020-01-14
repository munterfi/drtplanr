source("02_model_setup.R")

## Params
# min_dist = 100
n_stat = 10
n_seg = nrow(seg)
n_sim = 200

# Existing stations
idx_const <- st_nearest_feature(sta, seg)

# Random sample
idx <- c(sample_exclude(1:n_seg, n_stat, idx_const), idx_const)
idx_start <- idx

# Energy table
energy <- data.table(
  iteration = seq(0, n_sim),
  value = 0
)
energy[1, ]$value <- sum(calc_energy2(idx, seg, pop))

# Iterate
for (i in 2:(n_sim+1)) {
  e_old <- energy[i-1, ]$value
  idx_new_pos <- sample(1:n_stat, 1)
  idx_old <- idx[idx_new_pos]
  idx_new <- sample_exclude(1:n_seg, 1, idx)
  idx[idx_new_pos] <- idx_new
  e_new <- sum(calc_energy(idx, seg, pop))
  message(sprintf("Iteration: %s, e0: %s, e1: %s",
                  i-1, round(e_old, 1), round(e_new, 1)))
  if (e_old > e_new) {
    energy[i, ]$value <- e_new
  } else {
    energy[i, ]$value <- e_old
    idx[idx_new_pos] <- idx_old
  }
}

# Save result
results <- list(sta_const = seg[idx_const, ],
                sta_init = seg[idx_start[1:n_stat], ],
                sta_final = seg[idx[1:n_stat], ],
                energy = energy)
save(results, file = "result/model_results.RData")
