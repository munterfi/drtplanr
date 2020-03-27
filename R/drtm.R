### Model class definition
drt_drtm <- function(model_name, aoi, pop, n_vir, m_seg = 100,
                     calc_energy = e_walkDrive_pop) {
  aoi <- aoi %>% sf::st_transform(4326)
  pop <- pop %>% sf::st_transform(4326)
  
  # Get OSM data
  tmessage("Get data from OSM: Streets 'roa', bus stops and rail stations 'sta'")
  bb <- aoi %>% drt_osm_bb()
  roa <- dodgr::dodgr_streetnet(bbox = bb)
  
  # Stations
  sta_rail <-
    osmdata::opq(bbox = bb) %>%
    osmdata::add_osm_feature(key = "railway", value = "station") %>%
    osmdata::osmdata_sf() %>% 
    .$osm_points
  sta_rail$type = "rail"
  
  sta_bus <-
    osmdata::opq(bbox = bb) %>%
    osmdata::add_osm_feature(key = "highway", value = "bus_stop") %>%
    osmdata::osmdata_sf() %>% 
    .$osm_points
  sta_bus$type = "bus"
  
  sta <- rbind(
    sta_rail[, c("type", "geometry")],
    sta_bus[, c("type", "geometry")]
  )

  # Create routing graphs
  tmessage("Create routing graphs for 'foot' and 'mcar'")
  foot <- dodgr::weight_streetnet(roa, wt_profile = "foot")
  mcar <- dodgr::weight_streetnet(roa, wt_profile = "motorcar")
  
  # Mask layers to AOI
  tmessage("Mask layers 'roa', 'pop' and 'sta' by 'aoi'")
  roa <- roa[!roa$highway %in% c("footway", "path", "cycleway", "steps", "track", "service"), ]
  roa <- drt_mask(roa, aoi)
  pop <- drt_mask(pop, aoi)
  sta <- drt_mask(sta, aoi)
  
  # Split roads in segments
  tmessage("Extract possible station locations 'seg' from street segments")
  seg <- drt_roa_seg(roa, m_seg = 100)
  
  # Route driving times from all segments to the rail stations
  tmessage("Route driving times of 'seg' to the rail stations")
  car_time <- drt_route_matrix(
    orig = sta[sta$type == "rail", ],
    dest = seg,
    graph = mcar
  ) %>% 
    .[, .(travelTime = min(travelTime)), by = destIndex]
  seg$carTime <- car_time$travelTime
  seg <- seg[!is.na(seg$carTime), ]
  
  # Existing stations
  tmessage("Map stations 'sta' to segments 'seg' and set as constant")
  idx_const <- suppressMessages(
    sf::st_nearest_feature(sta, seg)
  )
  
  # Random sample
  n_seg <- nrow(seg)
  idx <- c(.sample_exclude(1:n_seg, n_vir, idx_const), idx_const)

  ## Create model obj
  model <- list(
    id = model_name,
    i = 0,
    idx_start = idx,
    idx_const = idx_const,
    idx = idx,
    e = data.table::data.table(
      iteration = 0,
      value = sum(calc_energy(idx, seg, pop, foot))
    ),
    params = list(
      n_sta = n_vir + length(idx_const),
      n_vir = n_vir,
      n_seg = nrow(seg),
      m_seg = m_seg,
      calc_energy = calc_energy
    ),
    route = list(
      foot = foot,
      mcar = mcar
    ),
    layer = list(
      aoi = aoi,
      roa = roa,
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


## OSM Layers
drt_osm_bb = function(x, ...) UseMethod("drt_osm_bb")

drt_osm_bb.sf <- function(aoi) {
  sf_bb <- aoi %>% sf::st_transform(4326) %>% sf::st_bbox()
  osm_bb <- matrix(sf_bb, 2)
  colnames(osm_bb) <- c("min", "max")
  rownames(osm_bb) <- c("x", "y")
  osm_bb
}


## Spatial
drt_mask = function(x, ...) UseMethod("drt_mask")

drt_mask.sf <- function(layer, aoi) {
  suppressMessages(
    suppressWarnings(
      sf::st_intersection(layer, sf::st_geometry(aoi))
    )
  )
}

drt_roa_seg = function(x, ...) UseMethod("drt_roa_seg")

drt_roa_seg.sf <- function(roa, m_seg) {
  seg <- 
    roa %>% 
    sf::st_union() %>% 
    sf::st_segmentize(units::set_units(m_seg, m)) %>%
    sf::st_cast("POINT") %>% 
    sf::st_as_sf()
  colnames(seg) <- c("geometry")
  sf::st_geometry(seg) <- "geometry"
  seg$id <- seq(1, nrow(seg))
  seg[, c("id", "geometry")]
}


### Routing
drt_route_matrix <- function(orig, dest, graph) {
  m <- dodgr::dodgr_times(
    graph = graph,
    from = sf::st_coordinates(orig),
    to = sf::st_coordinates(dest)
  )
  nn <- expand.grid(1:nrow(m), 1:ncol(m)) %>% data.table::data.table()
  colnames(nn) <- c("origIndex", "destIndex")
  nn$travelTime <- m[cbind(nn$origIndex, nn$destIndex)] %>% as.numeric()
  nn[order(nn$origIndex, nn$destIndex), ]
}


### Sampling
.sample_exclude <- function(x, size, const) {
  sampling <- TRUE
  while (sampling) {
    idx <- sample(x, size, replace = FALSE)
    if (any(idx %in% const)) {
      sampling <- TRUE
      #message("\r  Duplicate stations: Resampling...                          ")
    } else {
      sampling <- FALSE
    }
  }
  return(idx)
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
e_walkDrive_pop <- function(idx, seg, pop, graph) {
  # Route
  rts <- drt_route_matrix(seg[idx, ], pop, graph = graph)
  # Get nearest station
  nn <- rts[, .SD[which.min(travelTime)], by = list(destIndex)]
  nn$pop <- pop[nn$destIndex, ]$n
  nn$carTime <- seg[idx[nn$origIndex], ]$carTime
  return(nn[, sum((travelTime*carTime)/(pop)), by = list(origIndex)])
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
    e_new <- sum(obj$params$calc_energy(obj$idx, obj$layer$seg, obj$layer$pop, obj$route$foot))
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
drt_plot = function(x, ...) UseMethod("drt_plot")

drt_plot <- function(obj) {
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

drt_map = function(x, ...) UseMethod("drt_map")

drt_map.drtm <- function(obj) {
  tmessage("Print station map")
  const <- obj$layer$seg[obj$idx_const, ]
  const$station <- "Existing"; const$size <- 6
  start <- obj$layer$seg[obj$idx_start[!obj$idx_start %in% obj$idx_const], ]
  start$station <- "Initial"; start$size <- 3
  optim <- obj$layer$seg[obj$idx[!obj$idx %in% obj$idx_const], ]
  optim$station <- "Optimized"; optim$size <- 6
  stations <- rbind(const, start, optim)
  m <-
    mapview::mapview(
      obj$layer$aoi, alpha = 0.25, alpha.region = 0, color = "black", lwd = 2,
      legend = FALSE, layer.name = "AOI", label = "AOI", homebutton = TRUE
    ) +
    mapview::mapview(
      obj$layer$roa, alpha = 0.25, color = "black",
      legend = FALSE, layer.name = "Street network", homebutton = FALSE
    ) +
    mapview::mapview(
      obj$layer$seg, alpha = 0, alpha.region = 0.25,
      color = "black", col.region = "black", cex = 1,
      legend = FALSE, layer.name = "Segments", homebutton = FALSE
    ) +
    mapview::mapview(
      stations, alpha = 0, zcol = "station",
      cex = stations$size, col.regions = c("black", "blue", "red"),
      layer.name = "Stations", homebutton = FALSE
    )
  m
}


drt_save_graphics = function(x, ...) UseMethod("drt_save_graphics")

drt_save_graphics.drtm <- function(obj, path) {
  p <- drt_plot(obj)
  m <- drt_map(obj)
  tmessage("Export graphics")
  mapview::mapshot(m, file = paste0(path, "/%s_i%s_station_map.png" %>% sprintf(obj$id, obj$i)),
          remove_controls = c("zoomControl", "layersControl", "homeButton"))
  ggplot2::ggsave(paste0(path, "/%s_i%s_energy_plot.png" %>% sprintf(obj$id, obj$i)), plot = p,
         height = 125.984, width = 100, units = c("mm"), dpi = 150)
}
