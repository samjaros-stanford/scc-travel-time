# Options ######################################################################
options(java.parameters = "-Xmx3G", timeout = 9999)

# Setup ########################################################################
# Dependencies -----------------------------------------------------------------
library(here)
library(r5r)

# Set arguments ----------------------------------------------------------------
tract_origins <- readRDS("geo_data/tract_centers.rds")
blkgroup_origins <- readRDS("geo_data/blockgrp_centers.rds")
tx_destinations <- readRDS("geo_data/pharm_data.rds")
departure <- as.POSIXct("26-02-2025 16:30:00", format = "%d-%m-%Y %H:%M:%S", 
                        tz="America/Los_Angeles")

# Build out travel network -----------------------------------------------------
# TODO: Seems like there needs to be <4 GTFS feeds in the geo_data folder for it
#         to successfully save a network.dat file
r5r_core <- setup_r5(data_path = here("transit_analysis_data"), overwrite = T, verbose=T)

# Accessibility analysis #######################################################
# Experiencing an issue with Java throwing a null pointer exception
# TODO: Check for GTFS feed validity
access_60m_walk <- accessibility(r5r_core = r5r_core,
                                 origins = tract_origins,
                                 destinations = tx_destinations,
                                 opportunities_colnames = "records_here",
                                 mode = c("WALK"),
                                 max_walk_time = 60,
                                 max_trip_duration = 120,
                                 decay_function = "step",
                                 departure_datetime = departure,
                                 time_window = 60,
                                 cutoffs = 45)

# Travel time matrix ###########################################################
ttm <- travel_time_matrix(r5r_core = r5r_core,
                          origins = tract_origins,
                          destinations = tx_destinations,
                          mode = c("WALK", "TRANSIT"),
                          departure_datetime = departure,
                          max_walk_time = 60,
                          max_trip_duration = 120)

ttm_bike <- travel_time_matrix(r5r_core = r5r_core,
                               origins = tract_origins,
                               destinations = tx_destinations,
                               mode = c("WALK", "BIKE"),
                               departure_datetime = departure,
                               max_walk_time = 60,
                               max_trip_duration = 120)

# Cleanup ######################################################################
r5r::stop_r5(r5r_core)
rJava::.jgc(R.gc = T)
