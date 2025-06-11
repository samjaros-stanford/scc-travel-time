# Get GTFS feeds from Santa Clara County and adjacent transit providers
# Get street network data from Open Street Map
# Get elevation data from NASA
# Files to be used by r5r in transit_analysis

library(here)
library(tidyverse)

# GTFS Feeds ###################################################################
library(gtfstools)

# Get GTFS data from mobilitydatabase.org
# Need to specify specific times because GTFS feeds are often invalid
# Keep track of valid dates because we have to specify a valid datetime for travel time
agencies <- tribble(
  ~agency_id, ~agency_name, ~url,
  # samTrans - Feb 23 2025 to Jun 13 2025
  49,         "samTrans",   "https://files.mobilitydatabase.org/mdb-49/mdb-49-202502240014/mdb-49-202502240014.zip",
  # BART - Jan 13 2025 to Aug 9 2025
  53,         "BART",       "https://files.mobilitydatabase.org/mdb-53/mdb-53-202501130015/mdb-53-202501130015.zip",
  # CalTrain - Jan 27 2025 - Jul 30 2025
  54,         "Caltrain",   "https://files.mobilitydatabase.org/mdb-54/mdb-54-202501230102/mdb-54-202501230102.zip",
  # VTA - Jan 13 2025 - Aug 10 2025  ##### NEED UPDATED VERSION WHEN ERRORS GO AWAY #####
  57,         "VTA",        "https://files.mobilitydatabase.org/mdb-57/mdb-57-202504240107/mdb-57-202504240107.zip",
  # Marguerite - Aug 1 2024 to Oct 30 2025 
  59,         "Stanford Marguerite", "https://files.mobilitydatabase.org/mdb-59/mdb-59-202503130041/mdb-59-202503130041.zip",
  # MVGo - Feb 1 2025 - 
  2025,       "MVgo",       "https://files.mobilitydatabase.org/mdb-2025/mdb-2025-202502200003/mdb-2025-202502200003.zip"
)
# Get URLs from mobilitydatabase.org
# Agency ID's defined above
gtfs_sources <- agencies %>% 
  mutate(gtfs_file = paste0("raw_data/", agency_id, "_gtfs.zip"))
# --- Save .zips with mdb ID as filename ---------------------------------------
for(i in 1:nrow(gtfs_sources)){
  tryCatch(
    {
      download.file(url = gtfs_sources[i,]$url, 
                    destfile = gtfs_sources[i,]$gtfs_file,
                    method="auto")
      gtfs_sources[i, "download_successful"] <- T
    },
    error = function(e){
      print(e)
      gtfs_sources[i, "download_successful"] <<- F
    }
  )
}

# Validate GTFS format ---------------------------------------------------------
## Get validator & directories for validation results
gtfs_validator <- download_validator(here("gtfs_validation/"))
validation_out_dirs <- paste0("gtfs_validation/", gtfs_sources$agency_id, "_validation")

## Iterate through GTFSs and check validity
validation_results <- data.frame()
for(i in 1:nrow(gtfs_sources)){
  tryCatch(
    {
      validate_gtfs(gtfs = gtfs_sources[i,]$gtfs_file, 
                    output_path = validation_out_dirs[i], 
                    validator_path = gtfs_validator, 
                    pretty_json = T, 
                    html_preview = T)
      validation_results <- bind_rows(
        validation_results,
        jsonlite::fromJSON(paste0(validation_out_dirs[i], 
                                  "/report.json"))$notices %>%
          mutate(agency_id = gtfs_sources[i,]$agency_id,
                 agency_name = gtfs_sources[i,]$agency_name,
                 .before=1))},
    error = function(e){
      validation_results <<- bind_rows(
        validation_results,
        data.frame(agency_id = gtfs_sources[i,]$agency_id,
                   agency_name = gtfs_sources[i,]$agency_name,
                   code = as.character(e), 
                   severity = "NO GTFS", 
                   sampleNotices = NA)
      )
    })
}
## NOTE: Check to make sure the files at least have a GTFS format. You may need to
##   download gtfs.zip manually from a backup source

# Street Network ###############################################################
library(osmextract)

oe_get("Northern California", download_directory = here("raw_data"), 
       max_file_size = 6e8, force_download = T, download_only = T, 
       stringsAsFactors = F, skip_vectortranslate = T)

# The downloaded .osm.pbf file needs to be clipped to Santa Clara County
# Run osmconvert54.exe (from https://wiki.openstreetmap.org/wiki/Osmconvert)
# Run command osmconvert us-west-latest.osm.pbf -b=-122.212,36.875,-121.187,37.505 --out-pbf -o=us-west-latest.osm_01.pbf 

# Move the completed file into the geo_data folder

# Elevation Data ###############################################################
library(elevatr)
library(geojsonsf)
library(raster)

scc_area <- geojson_sf(read_file(here("raw_data/scc_area.geojson")))
scc_elev_raster <- get_elev_raster(scc_area, z=12)
writeRaster(scc_elev_raster, here("geo_data/scc_elev.tif"), overwrite=T)
