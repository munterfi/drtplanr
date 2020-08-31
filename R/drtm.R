#' Demand responsive transport model class.
#'
#' Creates a demand responsive transport representation of in an Area of
#' Interest (AOI). The constructor takes a polygon (aoi), point population
#' values (pop) and the number of virtual stations to plan as input. Then it
#' retrieves the street network in the area of interest from OSM. Based on the
#' OSM data a routing graph for walking and driving is created.
#'
#' @param model_name character, name of the drtm.
#' @param aoi sf, polygon of the Area of Interest (AOI).
#' @param pop sf, centroids of a hectaraster population dataset covering the full extent of the 'aoi' input (column name for population must be 'n').
#' @param n_vir numeric, number of the virtual stations to place.
#' @param m_seg numeric, resolution of the road segmentation in meters.
#' @param calc_energy function, energy calculation function.
#'
#' @return
#' A demand responsive transport model of class 'drtm'.
#'
#' @export
#'
#' @examples
#' # Example data
#' aoi <-
#'   sf::st_read(system.file("example.gpkg", package = "drtplanr"), layer = "aoi")
#'
#' pop <-
#'   sf::st_read(system.file("example.gpkg", package = "drtplanr"), layer = "pop")
#'
#' # Create model
#' m <- drt_drtm(
#'   model_name = "Jegenstorf",
#'   aoi = aoi, pop = pop,
#'   n_vir = 10, m_seg = 100
#' )
#' m
drt_drtm <- function(model_name, aoi, pop, n_vir, m_seg = 100,
                     calc_energy = calc_energy) {
  aoi <- aoi %>% sf::st_transform(4326)
  pop <- pop %>% sf::st_transform(4326)

  # Get OSM data
  tmessage("Get data from OSM: Streets 'roa'")
  bb <- aoi %>% drt_osm_bb()
  roa <- dodgr::dodgr_streetnet(bbox = bb)

  # Create routing graphs
  tmessage("Create routing graphs for 'foot', 'bicy' and 'mcar'")
  roa <- roa[!roa$highway %in% c("platform", "proposed", NA), ]
  walk <- dodgr::weight_streetnet(roa, wt_profile = "foot")
  bicy <- dodgr::weight_streetnet(roa, wt_profile = "bicycle")
  mcar <- dodgr::weight_streetnet(roa, wt_profile = "motorcar")

  # Mask layers to AOI
  tmessage("Mask layers 'roa', 'pop' and 'poi' by 'aoi'")
  roa <- roa[!roa$highway %in% c("footway", "path", "cycleway", "steps", "track", "service"), ]
  roa <- drt_mask(roa, aoi)
  pop <- drt_mask(pop, aoi)

  # Split roads in segments
  tmessage("Extract possible station locations 'seg' from street segments")
  seg <- drt_roa_seg(roa, m_seg = 100)

  # Random sample
  n_seg <- nrow(seg)
  idx <- sample(1:n_seg, n_vir, replace = FALSE) # c(.sample_exclude(1:n_seg, n_vir)) #, idx_const), idx_const)

  # Create model obj
  model <- list(
    id = model_name,
    i = 0,
    idx_start = idx,
    #idx_const = idx_const,
    idx = idx,
    e = data.table::data.table(
      iteration = 0,
      value = calc_energy(idx, seg, pop, walk, bicy)
    ),
    params = list(
      n_sta = n_vir, #+ length(idx_const),
      n_vir = n_vir,
      n_seg = nrow(seg),
      m_seg = m_seg,
      calc_energy = calc_energy
    ),
    route = list(
      bicy = bicy,
      walk = walk,
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

#' Print
#'
#' @param x drtplanr object, print information about package classes.
#' @param ... ...
#'
#' @return
#' None.
#'
#' @export
#'
#' @examples
#' # Example model
#' m <- drt_import(
#'   system.file("Jegenstorf_i1000.RData", package = "drtplanr")
#' )
#' print(m)
print.drtm <- function(x, ...) {
  div <- "========================================"
  bbox <- x$layer$aoi %>% sf::st_bbox()
  "%s%s\n%s '%s'\n
%-39s%-20s%-20s\n%-39s%20.0f%21.2f
%-39s%-20s%-20s\n%-39s%20.1f%21.1f
%-39s%-20s%-20s\n%-39s%20.0f%21.0f
%-39s%-20s%-21s\n%-39s%20.5f%21.5f
%-39s%20.5f%21.5f\n" %>%
    sprintf(
      div, div,
      "Demand-responsive transport model", x$id,
      "______________________________________", "i __________________", " dE/di ______________",
      "Iteration:", x$e[x$i+1, ]$iteration, diff(m$e$value) %>% mean() %>% round(2),
      "______________________________________", "initial ____________", " current ____________",
      "Energy:", x$e[1, ]$value %>% round(1), x$e[x$i+1, ]$value %>% round(1),
      "______________________________________", "existing ___________", " virtual ____________",
      "Stations:", length(x$idx_const),  x$params$n_sta - length(x$idx_const),
      "______________________________________", "lng ________________", " lat ________________",
      "BBox:                             min:", bbox[1],  bbox[2],
      "                                  max:", bbox[3],  bbox[4]
    ) %>%
    cat()
  invisible(x)
}


## OSM Layers

#' Get the box in the osmdata format.
#'
#' @param aoi, spatial object, object to retreive the bounding box from.
#'
#' @return
#' A matrix conaining the bbox in osmdata format.
#' @export
#'
#' @examples
#' # Example data
#' aoi <-
#'   sf::st_read(system.file("example.gpkg", package = "drtplanr"), layer = "aoi")
#'
#' drt_osm_bb(aoi)
drt_osm_bb = function(aoi) UseMethod("drt_osm_bb")

#' @export
drt_osm_bb.sf <- function(aoi) {
  sf_bb <- aoi %>% sf::st_transform(4326) %>% sf::st_bbox()
  osm_bb <- matrix(sf_bb, 2)
  colnames(osm_bb) <- c("min", "max")
  rownames(osm_bb) <- c("x", "y")
  osm_bb
}


## Spatial

#' Mask a spatial layer by a polygon
#'
#' @param layer spatial object to crop by the polygon.
#' @param aoi spatial object, polygon to mask to.
#'
#' @return
#' A masked sf.
#'
#' @export
#'
#' @examples
#' # Example data
#' aoi <-
#'   sf::st_read(system.file("example.gpkg", package = "drtplanr"), layer = "aoi")
#'
#' pop <-
#'   sf::st_read(system.file("example.gpkg", package = "drtplanr"), layer = "pop")
#'
#' drt_mask(pop, aoi)
drt_mask = function(layer, aoi) UseMethod("drt_mask")

#' @export
drt_mask.sf <- function(layer, aoi) {
  suppressMessages(
    suppressWarnings(
      sf::st_intersection(layer, sf::st_geometry(aoi))
    )
  )
}

#' Segmentize roads
#'
#' @param roa spatial object, contains the roads.
#' @param m_seg numeric, resoution for the length of the segemnts in meters.
#'
#' @return
#' A sf object containing the segementized road network points.
#' @export
#'
#' @examples
#' print("tbd.")
drt_roa_seg = function(roa, m_seg) UseMethod("drt_roa_seg")

#' @export
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


#' Routing
#'
#' @param orig sf, points of the origins.
#' @param dest sf, points of the destination.
#' @param graph dodgr routing graph, graph to route between the points.
#'
#' @return
#' A data.table containing the summaries of the routed connections.
#' @export
#'
#' @examples
#' print("tbd.")
drt_route_matrix <- function(orig, dest, graph) {
  m <- dodgr::dodgr_times(
    graph = graph,
    from = sf::st_coordinates(orig),
    to = sf::st_coordinates(dest)
  )
  nn <- data.table::data.table(expand.grid(1:nrow(m), 1:ncol(m)))
  colnames(nn) <- c("origIndex", "destIndex")
  nn$travelTime <- as.numeric(m[cbind(nn$origIndex, nn$destIndex)])
  nn[order(nn$origIndex, nn$destIndex), ]
}


## Sampling

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


## Energy functions

#' Global energy:
#'
#' @param idx numeric, indices of candidates ins 'seg'.
#' @param seg sf, road segments point locations.
#' @param pop sf, population point data.
#' @param walk dodgr routing graph, graph to route walking times between the points.
#' @param bicy dodgr routing graph, graph to route bicycle times between the points.
#'
#' @return
#' A data.table object containing the energies.
#'
#' @export
#'
#' @examples
#' print("tbd.")
calc_energy = function(idx, seg, pop, walk, bicy) {

  # rts_walk: Station to Pop
  rts_walk <- drt_route_matrix(seg[idx, ], pop, graph = walk)
  colnames(rts_walk) <- c("station", "raster", "walking_time")
  # rts_walk: Station to station
  rts_bicy <- drt_route_matrix(seg[idx, ], seg[idx, ], graph = bicy)
  colnames(rts_bicy) <- c("station_1", "station_2", "bicycle_time")

  # Local energy
  nn <- rts_walk[, .SD[which.min(walking_time)], by = list(raster)]
  nn$population <- pop[nn$raster, ]$n
  local_params <- nn[, list(population = sum(population), e_local = sum(population * walking_time)), by = list(station)]

  # Remove NAs
  rts_bicy[is.na(rts_bicy$bicycle_time), bicycle_time := max(rts_bicy$bicycle_time, na.rm = TRUE)*2]

  # Global energy
  tmp <- merge(
    rts_bicy,
    local_params,
    by.x = "station_1", by.y = "station")
  colnames(tmp) <- c("station_1","station_2", "bicycle_time", "population_1", "e_local_1")
  global <- merge(
    tmp,
    local_params,
    by.x = "station_2", by.y = "station")
  colnames(global) <- c("station_1","station_2", "bicycle_time", "population_1", "e_local_1", "population_2", "e_local_2")
  global[, c("s1", "s2", "s3") := list(
    population_1 * e_local_1,
    bicycle_time * population_1 * population_2,
    population_2 * e_local_2)]
  global[, s4 := s1 + s2 + s3]
  global <- global[station_1 != station_2, ]

  return(sum(global$s4))
}


## Interact with drtm class

#' Iterate
#'
#' Minimize the gloabal energy of a drtm model.
#'
#' @param obj, drtm, a drtm model.
#' @param n_iter numeric, number of iterations.
#' @param annealing boolean, apply annealing (alpha = 1/(i+1)) (default = TRUE).
#'
#' @return
#' A new drtm with 'n_iter' times more iterations.
#' @export
#'
#' @examples
#' # Example model
#' m <- drt_import(
#'   system.file("Jegenstorf_i1000.RData", package = "drtplanr")
#' )
#'
#' drt_iterate(m, 10)
drt_iterate = function(obj, n_iter, annealing = TRUE) UseMethod("drt_iterate")

#' @export
drt_iterate.drtm <- function(obj, n_iter, annealing = TRUE) {
  tmessage("Minimize the global energy of the model")
  # Set up alpha for annealing
  alpha <- if (annealing) function(x) 1/(x + 1) else function(x) 0
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
    e_new <- sum(
      obj$params$calc_energy(obj$idx, obj$layer$seg, obj$layer$pop, obj$route$walk, obj$route$bicy)
    )
    cat(sprintf("\r  Iteration: %s, e0: %s, e1: %s \r",
                i - 1, round(e_old, 1), round(e_new, 1)))
    if (e_old > e_new) {
      obj$e[i, ]$value <- e_new
    } else if (rbinom(n = 1, size = 1, prob = alpha(i))) {
      print("Annealing!")
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

#' Get energy function
#'
#' @param x, drtm, a drtm model.
#'
#' @return
#' The energy function of the model.
#'
#' @export
#'
#' @examples
#' # Example model
#' m <- drt_import(
#'   system.file("Jegenstorf_i1000.RData", package = "drtplanr")
#' )
#'
#' drt_energy(m)
drt_energy = function(x) UseMethod("drt_energy")

#' @export
drt_energy.drtm = function(x) {
  x$params$calc_energy
}

#' Set energy function
#'
#' @param x, drtm, a drtm model.
#' @param value function, energy function to calculate energy of the model.
#'
#' @return
#' A new drtm with the new energy.
#'
#' @export
#'
#' @examples
#' # Example model
#' m <- drt_import(
#'   system.file("Jegenstorf_i1000.RData", package = "drtplanr")
#' )
#'
#' drt_energy(m) <- calc_energy
`drt_energy<-` = function(x, value) UseMethod("drt_energy<-")

#' @export
`drt_energy<-.drtm` = function(x, value) {
  if (!is.function(energy))
    stop("The argument 'value' must be a function.")
  x$params$calc_energy <- value
  x
}

#' Export drtm
#'
#' @param obj drtm, a drtm model of the drtplanr.
#' @param path character, path to file.
#'
#' @return
#' None.
#'
#' @export
#'
#' @examples
#' \donttest{
#' # Example model
#' m <- drt_import(
#'   system.file("Jegenstorf_i1000.RData", package = "drtplanr")
#' )
#'
#' # Export to temporary dir
#' drt_export(m, path = tempdir())
#' }
drt_export = function(obj, path) UseMethod("drt_export")

#' @export
drt_export.drtm <- function(obj, path) {
  file_path <- file.path(path, paste0(obj$id, "_i", obj$i, ".RData"))
  tmessage("Export drtm to '%s'" %>% sprintf(file_path))
  save(obj, file = file_path)
}

#' Import drtm
#'
#' @param file_name character, path to file.
#'
#' @return
#' A drtm model.
#'
#' @export
#'
#' @examples
#' # Example model
#' m <- drt_import(
#'   system.file("Jegenstorf_i1000.RData", package = "drtplanr")
#' )
drt_import = function(file_name) UseMethod("drt_import")

#' @export
drt_import.character <- function(file_name) {
  drt <- .load_RData(file_name)
  drt
}

.load_RData <- function(file_name){
  load(file_name)
  get(ls()[ls() != "file_name"])
}

#' Plot energy curve
#'
#' @param obj drtm, a drtm model.
#'
#' @return
#' A ggplot2 plor object, containing the energy curve.
#'
#' @export
#' @examples
#' # Example model
#' m <- drt_import(
#'   system.file("Jegenstorf_i1000.RData", package = "drtplanr")
#' )
#'
#' # Plot
#' drt_plot(m)
drt_plot = function(obj) UseMethod("drt_plot")

#' @export
drt_plot <- function(obj) {
  tmessage("Print energy plot")
  if (nrow(obj$e) <= 1) {
    tmessage("Nothing to plot yet, use 'drt_iterate()' first")
    return(NULL)
  }
  p <-
    ggplot2::ggplot(obj$e, ggplot2::aes(x = iteration, y = value)) +
    ggplot2::geom_line() +
    ggplot2::xlab("Iteration") +
    ggplot2::ylab("Energy") +
    ggplot2::ggtitle("drtm: '%s', iterations: %s" %>% sprintf(obj$id, obj$i)) +
    ggplot2::theme_minimal()
  p
}

#' Plot station map
#'
#' @param obj drtm, a drtm model.
#'
#' @return
#' A mapview map object, containing the energy curve.
#' @export
#'
#' @examples
#' # Example model
#' m <- drt_import(
#'   system.file("Jegenstorf_i1000.RData", package = "drtplanr")
#' )
#'
#' drt_map(m)
drt_map = function(obj) UseMethod("drt_map")

#' @export
drt_map.drtm <- function(obj) {
  tmessage("Print station map")
  #const <- obj$layer$seg[obj$idx_const, ]
  #const$station <- "Existing"; const$size <- 6
  start <- obj$layer$seg[obj$idx_start, ]
  start$station <- "Initial"; start$size <- 3
  optim <- obj$layer$seg[obj$idx, ]
  optim$station <- "Optimized"; optim$size <- 6
  stations <- rbind(start, optim)
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
      cex = stations$size, col.regions = c("blue", "red"),
      layer.name = "Stations", homebutton = FALSE
    )
  m
}

#' Save graphics of model
#'
#' @param obj drtm, a drtm model.
#' @param path character, path to file.
#'
#' @return
#' None.
#'
#' @export
#'
#' @examples
#' #' # Example model
#' m <- drt_import(
#'   system.file("Jegenstorf_i1000.RData", package = "drtplanr")
#' )
#'
#' # Save to temp dir
#' drt_save_graphics(m, path = tempdir())
drt_save_graphics = function(obj, path) UseMethod("drt_save_graphics")

#' @export
drt_save_graphics.drtm <- function(obj, path) {
  p <- drt_plot(obj)
  m <- drt_map(obj)
  tmessage("Export graphics")
  mapview::mapshot(m, file = paste0(path, "/%s_i%s_station_map.png" %>% sprintf(obj$id, obj$i)),
                   remove_controls = c("zoomControl", "layersControl", "homeButton"))
  ggplot2::ggsave(paste0(path, "/%s_i%s_energy_plot.png" %>% sprintf(obj$id, obj$i)), plot = p,
                  height = 125.984, width = 100, units = c("mm"), dpi = 150)
}
