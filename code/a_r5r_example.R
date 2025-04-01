library(r5r)
library(sf)
library(data.table)
library(ggplot2)

data_path <- system.file("extdata/poa", package = "r5r")
list.files(data_path)

poi <- fread(file.path(data_path, "poa_points_of_interest.csv"))
head(poi)

points <- fread(file.path(data_path, "poa_hexgrid.csv"))

# sample points
sampled_rows <-  sample(1:nrow(points), 200, replace=TRUE)
points <- points[ sampled_rows, ]
head(points)

# Indicate the path where OSM and GTFS data are stored
r5r_core <- setup_r5(data_path = data_path)

# set departure datetime input
departure_datetime <- as.POSIXct("13-05-2019 14:00:00",
                                 format = "%d-%m-%Y %H:%M:%S")

# calculate accessibility
access <- accessibility(r5r_core = r5r_core,
                        origins = points,
                        destinations = points,
                        opportunities_colnames = c("schools", "healthcare"),
                        mode = c("WALK", "TRANSIT"),
                        departure_datetime = departure_datetime,
                        decay_function = "step",
                        cutoffs = 60
)

head(access)