# Options ######################################################################
options(java.parameters = "-Xmx5G", timeout = 999999)

# Setup ########################################################################
# Dependencies -----------------------------------------------------------------
library(here)
library(r5r)
library(sf)
library(tidyverse)

# Get geographies

# Set arguments ----------------------------------------------------------------
# Origins - tract centroids or grid
tract_origins <- readRDS("geo_data/tract_centers.rds") %>%
  left_join(readRDS("geo_data/tract_geos.rds"), by=join_by(id==fips)) %>%
  mutate(origin = "TRACT") %>%
  st_as_sf(crs = 4326)
# point_origins <- readRDS("geo_data/scc_points.rds") %>%
#   mutate(origin = "GRID") %>%
#   st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = F)
# Destinations - pharmacies
tx_destinations <- readRDS("geo_data/pharm_data.rds") %>%
  #r5r needs an opportunities column
  #id's need to be char
  mutate(opportunities = 1,
         id = as.character(id)) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = F)
# Departure datetime
departure <- as.POSIXct("16-04-2025 16:30:00", format = "%d-%m-%Y %H:%M:%S", 
                        tz="America/Los_Angeles")

# Build out travel network -----------------------------------------------------
# Need to add overwrite = T if anything changes to GTFS, road, or elevation
r5r_core <- setup_r5(data_path = here("transit_analysis_data"))

# Accessibility analysis #######################################################
# access_60m_walk <- accessibility(r5r_core = r5r_core,
#                                  origins = tract_origins,
#                                  destinations = tx_destinations,
#                                  mode = c("TRANSIT"),
#                                  max_walk_time = 60,
#                                  max_trip_duration = 120,
#                                  decay_function = "step",
#                                  departure_datetime = departure,
#                                  time_window = 60,
#                                  cutoffs = 45)

# Travel time matrix ###########################################################
top_3_tt <- function(origin_df, modes, max_dur=240) {
  ttm <- travel_time_matrix(r5r_core = r5r_core,
                            origins = st_drop_geometry(origin_df),
                            destinations = st_drop_geometry(tx_destinations),
                            mode = modes,
                            departure_datetime = departure,
                            time_window = 30,
                            max_walk_time = 120,
                            max_trip_duration = max_dur)
  
  ttm %>%
    # Get only top 3 travel times
    group_by(from_id) %>%
    arrange(travel_time_p50) %>%
    mutate(time_rank = row_number()) %>%
    filter(time_rank < 4) %>%
    ungroup() %>%
    # Flip data for saving
    pivot_wider(id_cols = from_id,
                names_from = time_rank,
                names_prefix = "tt_option_",
                values_from = travel_time_p50) %>%
    # Add source dataset to get NA's
    rename(id = from_id) %>%
    full_join(origin_df, by="id") %>%
    st_as_sf() %>%
    # Add indicator column for time
    mutate(method = modes[1])
}

transit_top_3_tt <- top_3_tt(tract_origins, c("TRANSIT", "WALK"))
saveRDS(transit_top_3_tt, here("data/tract_tts_transit.rds"))

tract_tts_v2 <- tract_tts %>%
  filter(method != "TRANSIT") %>%
  bind_rows(transit_top_3_tt)
saveRDS(tract_tts_v2, here("data/tract_tts_v3.rds"))


tract_tts <- bind_rows(
  top_3_tt(tract_origins, c("TRANSIT", "WALK")),
  top_3_tt(tract_origins, c("CAR")),
  top_3_tt(tract_origins, c("BICYCLE")),
  top_3_tt(tract_origins, c("WALK"))
)
saveRDS(tract_tts, here("data/tract_tts_v2.rds"))

tt_point_transit <- top_3_tt(point_origins, c("TRANSIT", "WALK"))
tt_point_car <- top_3_tt(point_origins, c("CAR"), max_dur=60)
tt_point_bike <- top_3_tt(point_origins, c("BICYCLE"))
tt_point_walk <- top_3_tt(point_origins, c("WALK"))

saveRDS(bind_rows(tt_point_transit,
                  tt_point_car,
                  tt_point_bike,
                  tt_point_walk), 
        here("data/point_tts.rds"))

tract_tts_v2 %>%
  st_drop_geometry() %>%
  pivot_wider(id_cols = id,
              names_from = method,
              values_from = tt_option_1) %>%
  filter(TRANSIT > WALK) %>%
  View

# Cleanup ######################################################################
r5r::stop_r5(r5r_core)
rJava::.jgc(R.gc = T)
