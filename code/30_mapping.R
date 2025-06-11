
# Map travel time to opioid treatment centers across Santa Clara County

library(here)
library(tidyverse)
library(ggpattern)
library(ggspatial)

# Options ======================================================================
tile.dir <- here("geo_data/tiles/")
save.dir <- here("images/")
img.width <- 5 # inches

# Data =========================================================================
tract_tts <- readRDS(here("data/tract_tts_v2.rds")) %>%
  mutate(tt_diff = tt_option_3 - tt_option_1) %>%
  mutate(across(starts_with("tt_"), as.double)) %>%
  replace_na(replace = list(tt_option_1 = Inf,
                            tt_option_2 = Inf,
                            tt_option_3 = Inf,
                            tt_diff = Inf))

# Mapping ======================================================================
map_tts <- function(tt_var, tt_method, time_cutoffs=c(30,60), do.save=F) {
  # Create labels based on the time cutoffs provided
  time_labels <- c()
  for(i in 1:(length(time_cutoffs)+1)){
    if(i==1) {
      time_labels <- c(time_labels, paste0("<=", time_cutoffs[1], " min"))
    } else if(i==(length(time_cutoffs)+1)) {
      time_labels <- c(time_labels, paste0(">", time_cutoffs[i-1], " min"))
    } else {
      time_labels <- c(time_labels, paste0(time_cutoffs[i-1], "-", 
                                           time_cutoffs[i], " min"))
    }
  }
  
  # Get dataset ready for mapping including changing NA's (no valid travel time found) to Inf
  # Bin travel time variable to time cutoffs
  d <- tract_tts %>%
    filter(method==tt_method) %>%
    mutate(time_bin = cut(.data[[tt_var]],
                          breaks = c(-Inf, time_cutoffs, Inf),
                          labels = time_labels)) %>%
    mutate(time_bin = if_else(is.na(time_bin),
                              "No valid options",
                              time_bin))
  
  # Get colors for the binned time variable
  time_colors <- c(rev(RColorBrewer::brewer.pal(length(time_labels), "Spectral")),
                   NA)
  names(time_colors) <- c(time_labels, "No valid options")
  
  # Decide on legend title
  legend_title <- ifelse(tt_var=="tt_diff", "Time Increase", "Travel Time")
  
  p <- ggplot(d) +
    # Use cartolight basemap from openstreetmaps
    # Zoomin=0 gives higher resolution basemap, cache to be kind to osm
    annotation_map_tile(type="cartolight", zoomin=0, cachedir=tile.dir) +
    geom_sf(aes(fill = time_bin), 
            alpha=0.5) +
    annotation_scale() + # Add map scale
    annotation_north_arrow(location = "bl",
                           height = unit(0.75, "cm"),
                           width = unit(0.5, "cm"),
                           pad_x = unit(0.15, "cm"),
                           pad_y = unit(0.6, "cm"),
                           style = north_arrow_fancy_orienteering) +
    # Coloring
    scale_fill_manual(name = legend_title,
                      values = time_colors, 
                      na.value=NA, 
                      breaks = names(time_colors)) +
    theme(axis.text = element_blank(),
          axis.ticks = element_blank(),
          legend.background = element_blank(),
          legend.justification = c(1, 1),
          legend.key = element_rect(color = "transparent"),
          legend.key.size = unit(0.5, "cm"),
          legend.position = "inside",
          legend.position.inside = c(1, 1),
          legend.text = element_text(size = 8),
          legend.title = element_text(size = 10))
  
  if(do.save){
    ggsave(filename = paste0(save.dir, "/", tt_var, "_", tt_method, "_map_15.png"),
           plot = p, width = img.width, height = img.width/1.3, units = "in")
  }
  
  return(p)
}

map_specs <- expand.grid(outcome = c("tt_option_1", "tt_diff"),
                         method = c("WALK", "TRANSIT", "BICYCLE", "CAR"),
                         stringsAsFactors = F)

mod_results <- mapply(map_tts,
                      tt_var = map_specs$outcome,
                      tt_method = map_specs$method,
                      time_cutoffs = list(c(15,30,60)),
                      do.save = T,
                      SIMPLIFY = F)


# Graveyard ====================================================================

# Get percentile values for scale coloring
#   ecdf returns a function that will give us the percentile of a given value
#   The percentile of that value between the min and max represents where on
#   the scale that color should be
# find_ptile <- ecdf(min(d[[tt_var]], na.rm=T):max(d[[tt_var]], na.rm=T))
# # Assign values 1-0 that will define where on the scale different colors should go
# tt_colors <- c(
#   1,
#   find_ptile(75),
#   find_ptile(70),
#   find_ptile(60),
#   find_ptile(50),
#   find_ptile(40),
#   find_ptile(30),
#   find_ptile(28),
#   find_ptile(26),
#   find_ptile(24),
#   0
# )
# # Name the values with the colors
# names(tt_colors) <- RColorBrewer::brewer.pal(11, "PRGn")

# Playing around with patterns
# geom_sf_pattern(aes(fill = time_bin,
#                     pattern = time_bin),
#                 alpha = 0.5) +
#scale_pattern_type_discrete(choices = c("none", "none", "none", "fishscales")) +
#scale_pattern_discrete(choices = c("none", "none", "none", "circle")) +
