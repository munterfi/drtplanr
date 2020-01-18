library(sf)
library(data.table)
library(hereR)
set_key(jsonlite::read_json("config.json")$here$key)

# HERE API: Station, isoline and traffic
message(Sys.time(), " Send requests to HERE APIs: Geocode, isoline and stations")
geo <- geocode("Jegenstorf Bahnhof RBS, Bern, Schweiz") %>% st_transform(2056)
sta <- station(geo, radius = 2000, results = 50) %>% st_transform(2056)
st_write(sta, "prep/station.gpkg", delete_dsn = TRUE, quiet	= TRUE)
iso <- isoline(sta[2, ], mode = "car", range = 3*60, traffic = FALSE) %>% st_transform(2056)
st_write(iso, "prep/isoline.gpkg", delete_dsn = TRUE, quiet	= TRUE)
tra <- traffic(iso, product = "flow") %>% st_transform(2056)
st_write(tra, "prep/traffic.gpkg", delete_dsn = TRUE, quiet	= TRUE)

## Road network
message(Sys.time(), " Read OSM street layer")
roa <- st_read("data/osm/gis_osm_roads_free_1.shp", quiet = TRUE) %>% st_transform(2056)
roa <- st_intersection(iso, roa)
st_write(roa, "prep/road.gpkg", delete_dsn = TRUE, quiet	= TRUE)

## Split roads in segments
message(Sys.time(), " Extract possible station locations from street segemnts")
seg <- 
  roa[!roa$fclass %in% c("footway", "cycleway", "steps"), ] %>%
  st_union() %>% 
  st_segmentize(units::set_units(50, m)) %>%
  st_cast("POINT") %>% 
  st_as_sf()

# Route drive times
# OLD: drive_time <- route(seg, sta[2, ], mode = "car")
# 
# NEW: but still buggy, hereR::route_matrix is fixed, please use version > 0.3.0
message(Sys.time(), " HERE Routing API: Calculate walking times to the station")
drive_time <- route_matrix(
  origin = sta[2, ][rep(seq_len(nrow(sta[2, ])), each = nrow(seg)), ],
  destination = seg,
  mode = "car"
)
seg$carTime <- drive_time[1:nrow(seg), ]$travelTime
#st_write(seg, "prep/segment.gpkg", delete_dsn = TRUE, quiet	= TRUE)

## STATPOP 2018
message(Sys.time(), " Read and process STATOP layer")
pop <- fread("data/statpop/STATPOP2018G.csv")
pop$x <- pop$E_KOORD + 50
pop$y <- pop$N_KOORD + 50
pop$n <- pop$B18BTOT
pop <-
  pop[, c("x", "y", "n")] %>% 
  as.data.frame() %>% 
  st_as_sf(coords = c("x", "y")) %>% 
  st_set_crs(2056) %>% 
  st_intersection(iso)
st_write(pop, "prep/statpop.gpkg", quiet	= TRUE)

