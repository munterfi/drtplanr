### Model class definition
drt_model <- function(model_name, n_sta, sta, seg, pop, calc_energy = e_walkDrive_pop, router = NULL) {
  tmessage("Initialize model '%s'" %>% sprintf(model_name))
  if (is.null(router)) {
    router <- "NA"
    message("Pseudo-routing with sf::st_distance() --> For valid results, set up here or OSRM!")
  }
  # Existing stations
  idx_const <- sf::st_nearest_feature(sta, seg)
  
  # Random sample
  n_seg <- nrow(seg)
  idx <- c(.sample_exclude(1:n_seg, n_sta, idx_const), idx_const)

  ## Create model obj
  model <- list(
    id = model_name,
    i = 0,
    idx_start = idx,
    idx_const = idx_const,
    idx = idx,
    e = data.table::data.table(
      iteration = 0,
      value = sum(calc_energy(idx, seg, pop, router))
    ),
    params = list(
      n_sta = n_sta,
      n_seg = nrow(seg),
      router = router, 
      calc_energy = calc_energy
    ),
    data = list(
      seg = seg,
      pop = pop
    )
  )
  attr(model, 'class') <- 'drtm'
  model
}

print.drtm <- function(obj) {
  cat(
    sprintf(
      "Demand-responsive transport model '%s'\nIteration: %s\nInitial energy: %s\nCurrent energy: %s\nExisting stations: %s\nVirtual stations: %s",
      obj$id, obj$e[obj$i+1, ]$iteration,
      obj$e[1, ]$value %>% round(1), obj$e[obj$i+1, ]$value %>% round(1),
      length(obj$idx_const), obj$params$n_sta - length(obj$idx_const)
    )
  )
  invisible(obj)
}


### Sampling
.sample_exclude <- function(x, size, const) {
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


### Routing
drt_route_matrix <- function(orig, dest, mode, router) {
  if (router == "here") {
    nn <- hereR::route_matrix(orig, dest, mode = mode) %>%
      as.data.table()
  } else {
    m <- sf::st_distance(orig, dest, by_element = FALSE)
    nn <- expand.grid(1:nrow(m), 1:ncol(m)) %>% data.table()
    colnames(nn) <- c("origIndex", "destIndex")
    nn$travelTime <- m[cbind(nn$origIndex, nn$destIndex)] %>% as.numeric()
    nn[order(nn$origIndex, nn$destIndex), ]
  }
}


### Energy functions
# Global energy: sum(walk/pop)
e_walk_pop <- function(idx, seg, pop, router) {
  # Route
  rts <- drt_route_matrix(seg[idx, ], pop, mode = "pedestrian", router = router)
  # Get nearest station
  nn <- rts[, .SD[which.min(travelTime)], by = list(destIndex)]
  nn$pop <- pop[nn$destIndex, ]$n
  return(nn[, sum(travelTime/pop), by = list(origIndex)])
}

# Global energy: sum((walk+5*drive)/pop)
e_walkDrive_pop <- function(idx, seg, pop, router) {
  # Route
  rts <- drt_route_matrix(seg[idx, ], pop, mode = "pedestrian", router = router)
  # Get nearest station
  nn <- rts[, .SD[which.min(travelTime)], by = list(destIndex)]
  nn$pop <- pop[nn$destIndex, ]$n
  nn$carTime <- seg[idx[nn$origIndex], ]$carTime
  return(nn[, sum((travelTime+5*carTime)/(pop)), by = list(origIndex)])
}


### Iterate
drt_iterate = function(x, ...) UseMethod("drt_iterate")

drt_iterate.drtm <- function(obj, n_iter) {
  tmessage("Minimize the global energy of the model")
  # Expand energy dt
  obj$e <- rbind(
    obj$e,
    data.table::data.table(
      iteration = seq(obj$i + 1, obj$i + n_iter),
      value = 0
    )
  )
  for (i in (obj$i + 2):((obj$i + n_iter + 1))) {
    e_old <- obj$e[i - 1, ]$value
    idx_new_pos <- sample(1:obj$params$n_sta, 1)
    idx_old <- obj$idx[idx_new_pos]
    idx_new <- .sample_exclude(1:obj$params$n_seg, 1, obj$idx)
    obj$idx[idx_new_pos] <- idx_new
    e_new <- sum(obj$params$calc_energy(obj$idx, obj$data$seg, obj$data$pop, obj$params$router))
    cat(sprintf("\r  Iteration: %s, e0: %s, e1: %s \r",
                i - 1, round(e_old, 1), round(e_new, 1)))
    if (e_old > e_new) {
      obj$e[i, ]$value <- e_new
    } else {
      obj$e[i, ]$value <- e_old
      obj$idx[idx_new_pos] <- idx_old
    }
  }
  tmessage("Model run completed (iterations: %s, e0: %s, e1: %s)" %>%
             sprintf(n_iter,
                     obj$e[obj$i + 1, ]$value %>% round(1),
                     obj$e[obj$i + n_iter + 1, ]$value %>% round(1)))
  obj$i <- obj$i + n_iter
  obj
}


## Export
drt_export = function(x, ...) UseMethod("drt_export")

drt_export.drtm <- function(obj, path) {
  file_path <- file.path(path, paste0(obj$id, "_i", obj$i, ".RData"))
  tmessage("Export drtm to '%s'" %>% sprintf(file_path))
  save(obj, file = file_path)
}


## Import
drt_import = function(x, ...) UseMethod("drt_import")

drt_import.character <- function(file_name) {
  drt <- .load_RData(file_name)
  drt
}

.load_RData <- function(file_name){
  load(file_name)
  get(ls()[ls() != "file_name"])
}


### Graphics
drt_energy_plot = function(x, ...) UseMethod("drt_energy_plot")

drt_energy_plot.drtm <- function(obj) {
  tmessage("Print energy plot")
  p <-
    ggplot2::ggplot(obj$e, ggplot2::aes(x = iteration, y = value)) +
    ggplot2::geom_line() + 
    ggplot2::xlab("Iteration") +
    ggplot2::ylab("Energy") +
    ggplot2::ggtitle("drtm: '%s', iterations: %s" %>% sprintf(obj$id, obj$i)) +
    ggplot2::theme_minimal()
  p
}

drt_station_map = function(x, ...) UseMethod("drt_station_map")

drt_station_map.drtm <- function(obj) {
  tmessage("Print station map")
  const <- obj$data$seg[obj$idx_const, ]
  const$station <- "Existing"; const$size <- 6
  start <- obj$data$seg[obj$idx_start[!obj$idx_start %in% obj$idx_const], ]
  start$station <- "Initial"; start$size <- 3
  optim <- obj$data$seg[obj$idx[!obj$idx %in% obj$idx_const], ]
  optim$station <- "Optimized"; optim$size <- 6
  stations <- rbind(const, start, optim)
  m <-
    mapview::mapview(
      stations, alpha = 0, zcol = "station",
      cex = stations$size, col.regions = c("black", "blue", "red"),
      layer.name = "Stations"
    )
  m
}


drt_save_graphics = function(x, ...) UseMethod("drt_save_graphics")

drt_save_graphics.drtm <- function(obj, path) {
  p <- drt_energy_plot(obj)
  m <- drt_station_map(obj)
  tmessage("Export graphics")
  mapview::mapshot(m, file = paste0(path, "/%s_i%s_station_map.png" %>% sprintf(obj$id, obj$i)),
          remove_controls = c("zoomControl", "layersControl", "homeButton"))
  ggplot2::ggsave(paste0(path, "/%s_i%s_energy_plot.png" %>% sprintf(obj$id, obj$i)), plot = p,
         height = 125.984, width = 100, units = c("mm"), dpi = 150)
}
