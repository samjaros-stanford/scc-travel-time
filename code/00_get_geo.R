# Retrieve needed geographic data for census tracts and block groups and save
# them for future use. Remove unneeded columns from imported data to save space.
#   Boundaries from Santa Clara County Open Data Portal
#   Population-weighted centroids from Census "Centers of Population"

# Import #######################################################################
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
  select(id, pop, lat, lon)

# 2020 Block Group population centers from census.gov
cen_blockgrp_centers <- read_csv("https://www2.census.gov/geo/docs/reference/cenpop2020/blkgrp/CenPop2020_Mean_BG06.txt") %>%
  filter(COUNTYFP == "085") %>%
  mutate(id = paste0(STATEFP, COUNTYFP, TRACTCE, BLKGRPCE)) %>%
  rename(pop = POPULATION,
         lat = LATITUDE,
         lon = LONGITUDE) %>%
  select(id, pop, lat, lon)

# Prescribers ------------------------------------------------------------------
# Currently using example geographic data from the SAMHSA treatment locator
raw_pharm <- read_csv("raw_data/unique_tx_providers.csv") %>%
  select(-1)

# Processing ###################################################################
# Process prescribers for use as destination in travel time analysis
clean_pharm <- raw_pharm %>%
  mutate(id = as.character(row_number())) %>%
  rename(lat = latitude,
         lon = longitude)

# Export #######################################################################
# Save as RDS for use in this project
saveRDS(sc_tracts, file="geo_data/tract_geos.rds")
saveRDS(sc_blockgrps, file="geo_data/blockgrp_geos.rds")
saveRDS(cen_tract_centers, file="geo_data/tract_centers.rds")
saveRDS(cen_blockgrp_centers, file="geo_data/blockgrp_centers.rds")
saveRDS(clean_pharm, "geo_data/pharm_data.rds")
