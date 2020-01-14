library(sf)
library(parallel)
library(data.table)
library(mapview)

# HERE API: Station, isoline and traffic
library(hereR)
set_key("IgV22-xAtzbf4N9wGd96TLJyoNWeB7FzKQuFpHNcA94")
geo <- geocode("Jegenstorf Bahnhof RBS, Bern, Schweiz") %>% st_transform(2056)
sta <- station(geo, radius = 2000, results = 50) %>% st_transform(2056)
st_write(sta, "data/station.gpkg")
iso <- isoline(sta[2, ], mode = "car", range = 3*60, traffic = FALSE) %>% st_transform(2056)
st_write(iso, "data/isoline.gpkg")
tra <- traffic(iso, product = "flow") %>% st_transform(2056)
st_write(tra, "data/traffic.gpkg")

## Road network
roa <- st_read("/Users/Merlin/Desktop/station-planning/data/switzerland-latest-free/gis_osm_roads_free_1.shp") %>% st_transform(2056)
roa <- st_intersection(iso, rds)
st_write(roa, "data/roads.gpkg")

## Read data
sta <- st_read("data/station.gpkg")
iso <- st_read("data/isoline.gpkg")
tra <- st_read("data/traffic.gpkg")
roa <- st_read("data/road.gpkg")

## Split roads in segments
seg <- 
  roa %>%
  st_segmentize(units::set_units(10,m)) %>%
  st_cast("POINT")
