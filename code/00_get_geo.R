# Retrieve needed geographic data for census tracts and block groups and save
# them for future use. Remove unneeded columns from imported data to save space.
#   Boundaries from Santa Clara County Open Data Portal
#   Population-weighted centroids from Census "Centers of Population"

library(here)
library(jsonlite)
library(sf)
library(tidyverse)

# Import #######################################################################
# Census -----------------------------------------------------------------------
# 2020 Tract data from SCC Open Data Portal
sc_tracts <- read_sf("https://data.sccgov.org/resource/kx4a-536x.geojson") %>%
  select(geoid, geometry) %>%
  rename(fips = geoid)

# 2020 Block Group data from SCC Open Data Portal
sc_blockgrps <- read_sf("https://data.sccgov.org/resource/sc5q-miaq.geojson?$limit=2000") %>%
  select(geoid, geometry) %>%
  rename(fips = geoid)

# 2020 Tract population centers from census.gov
cen_tract_centers <- read_csv("https://www2.census.gov/geo/docs/reference/cenpop2020/tract/CenPop2020_Mean_TR06.txt") %>%
  filter(COUNTYFP == "085") %>%
  mutate(id = paste0(STATEFP, COUNTYFP, TRACTCE)) %>%
  rename(pop = POPULATION,
         lat = LATITUDE,
         lon = LONGITUDE) %>%
  select(id, pop, lat, lon) %>%
  # Manually adjust tract centroid that ends up in the middle of a field
  mutate(lat = if_else(id=="06085503124", 37.28375, lat),
         lon = if_else(id=="06085503124", -121.86933, lon))

# 2020 Block Group population centers from census.gov
cen_blockgrp_centers <- read_csv("https://www2.census.gov/geo/docs/reference/cenpop2020/blkgrp/CenPop2020_Mean_BG06.txt") %>%
  filter(COUNTYFP == "085") %>%
  mutate(id = paste0(STATEFP, COUNTYFP, TRACTCE, BLKGRPCE)) %>%
  rename(pop = POPULATION,
         lat = LATITUDE,
         lon = LONGITUDE) %>%
  select(id, pop, lat, lon)

# Pharmacies- ------------------------------------------------------------------
# Get pharmacies in Santa Clara County in .json from Overpass (OpenStreeMaps API)
raw_pharm <- read_json(here("raw_data/osm_pharmacies.json"))$elements

get_geo <- function(osm_list) {
  # No-name bussiness means exclude unless there's a valid operator
  # If there's a valid operator, use it as the name as this means it's a pharmacy
  #   inside of a health care building
  if(is.null(osm_list$tags$name)) {
    if(is.null(osm_list$tags$operator)) {
      print(paste0("Excluding id: ", osm_list$id))
      return(NULL)
    } else {
      osm_list$tags$name <- osm_list$tags$operator
    }
  }

  if(osm_list$type == "node") {
    # Nodes are single points in OSM
    data.frame(name = osm_list$tags$name,
               id = osm_list$id,
               lon = osm_list$lon,
               lat = osm_list$lat)
  } else if(osm_list$type == "way") {
    # Way indicates the whole building footprint is stored in OSM
    # Keep first point
    data.frame(name = osm_list$tags$name,
               id = osm_list$nodes[[1]],
               lon = osm_list$geometry[[1]]$lon,
               lat = osm_list$geometry[[1]]$lat)
  }
}

all_pharm <- sapply(raw_pharm, get_geo) %>%
  bind_rows() %>%
  # Remove optometrists, duplicate pharmacies within a hospital
  # and two others that are closed/a headquarters of a drug delivery company
  filter(!grepl("Optometr|[0-9]{3} Pharmacy$|Pharmaca|Headquarters", name))

# Point Grid ###################################################################
scc_full <- st_union(sc_tracts) %>%
  st_set_crs(4326) %>%
  st_as_sf()

scc_points <- st_make_grid(scc_full, what="centers", n=75) %>%
  st_as_sf() %>%
  st_filter(scc_full) %>%
  mutate(id = as.character(row_number()),
         lon = st_coordinates(.)[,1],
         lat = st_coordinates(.)[,2]) %>%
  st_drop_geometry()


# Export #######################################################################
# Save as RDS for use in this project
saveRDS(sc_tracts, file="geo_data/tract_geos.rds")
saveRDS(sc_blockgrps, file="geo_data/blockgrp_geos.rds")
saveRDS(cen_tract_centers, file="geo_data/tract_centers.rds")
saveRDS(cen_blockgrp_centers, file="geo_data/blockgrp_centers.rds")
saveRDS(scc_points, file="geo_data/scc_points.rds")
saveRDS(all_pharm, "geo_data/pharm_data.rds")
