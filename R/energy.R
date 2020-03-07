## Energy fucntions

# Global energy: sum(walk/pop)
e_walk_pop <- function(idx, seg, pop) {
  # Route with HERE
  rts <-
    route_matrix(seg[idx, ], pop, mode = "pedestrian") %>%
    as.data.table()
  # Get nearest station
  nn <- rts[, .SD[which.min(travelTime)], by = list(destIndex)]
  nn$pop <- pop[nn$destIndex, ]$n
  return(nn[, sum(travelTime/pop), by = list(origIndex)])
}

# Global energy: sum(walk*drive/pop)
e_walkDrive_pop <- function(idx, seg, pop) {
  # Route with HERE
  rts <-
    route_matrix(seg[idx, ], pop, mode = "pedestrian") %>%
    as.data.table()
  # Get nearest station
  nn <- rts[, .SD[which.min(travelTime)], by = list(destIndex)]
  nn$pop <- pop[nn$destIndex, ]$n
  nn$carTime <- seg[idx[nn$origIndex], ]$carTime
  return(nn[, sum((travelTime+5*carTime)/(pop)), by = list(origIndex)])
}