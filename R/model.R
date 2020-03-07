## Model

# Sampling
sample_exclude <- function(x, size, const) {
  sampling <- TRUE
  while (sampling) {
    idx <- sample(x, size, replace = FALSE)
    if (any(idx %in% const)) {
      message("\r  Duplicate stations: Resampling...                          ")
    } else {
      sampling <- FALSE
    }
  }
  return(idx)
}