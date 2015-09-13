
## exploratory-data-analysis.R ##

library(dplyr)
library(ggvis)
library(ggplot2)
library(ggmap)     # install.packages('ggmap')
library(seasonal)  # install.packages('seasonal')

# load or create the data file --------------------------------------------

DATA_FNAME <- 'water_data.RData'

if (file.exists(DATA_FNAME)) {
  load(DATA_FNAME)
} else {
  file_list <- list.files('./data')
  water_data <- data.frame()
  
  for (file in file_list) {
    load(paste0('./data/', file))   # loads a data.frame called state_data
    water_data <- rbind(water_data, state_data)
    rm(state_data)
  }
  save(water_data, file=DATA_FNAME)
  gc()
}

water_data$feet_below_surface <- -1 * water_data$feet_below_surface

# =========================================================================

# goal: find sites that have observations for every year of the data set.
#
years <- unique(format(water_data$date_time,'%Y'))

sites_of_interest <- 
  water_data %>% 
  group_by(site_code, yr=format(date_time, '%Y')) %>%
  summarize(count = n())

sites_of_interest <-
  sites_of_interest %>%
  filter(yr %in% c(1965, 2014) & count > 4) %>%
  select(site_code, yr)

sites_of_interest <- 
  sites_of_interest %>%
  group_by(site_code) %>%
  summarize(count = n()) %>%
  filter(count == 2) %>%
  select(site_code)
sites_of_interest <- factor(sites_of_interest$site_code)

water_data %>%
  filter(site_code %in% sites_of_interest) %>%
  ggvis(~date_time, ~feet_below_surface, fill = ~factor(site_name)) %>%
  layer_points(size:=4) %>%
  group_by(site_code) %>%
  layer_smooths()

# =========================================================================


# where are the sites selected above located?
#
site_locations <-
  water_data %>%
  filter(site_code %in% sites_of_interest) %>%
  group_by(site_code, site_name, site_latitude, site_longitude) %>%
  summarize(obs_count = n())

# map_center_lon = -72
# map_center_lat = 43.5
# 
# map <- get_map(location = c(lon = map_center_lon,
#                             lat = map_center_lat),
#                maptype = 'roadmap',
#                zoom = 8)
# 
# ggmap(map) +
#   geom_point(data = site_locations,
#              aes(x = site_longitude,
#                  y = site_latitude),
#              size = 5) +
#   geom_text(data = site_locations,
#             aes(x = site_longitude + 0.04,
#                 y = site_latitude + 0.04,
#                 label = site_name,
#                 hjust = 0))


# =========================================================================

nsite_codes <- head(site_locations$site_code, 5)

dat <-
  water_data %>%
  mutate(yr = format(date_time, "%Y"), mon = format(date_time, "%m")) %>%
  filter(site_code %in% nsite_codes & date_time >= '1970-01-01') 

dat$site_name <- factor(dat$site_name)

dots_dat <- 
  dat %>%
  group_by(site_name, yr, mon) %>%
  summarize(feet_below_surface = min(feet_below_surface, na.rm = T)) # min is max(negative) in this case

medians_dat <-
  dat %>%
  group_by(site_name) %>%
  summarize(medn = median(feet_below_surface, na.rm = T))

ggplot(dots_dat, aes(x = mon, y = feet_below_surface, fill = site_name), color = 'gray') +
  geom_hline(data = medians_dat, aes(yintercept = medn), color = 'darkgray') +
  geom_point(size = 2, shape = 21) +
  facet_grid(site_name ~ yr)  +
  theme_bw() + 
  theme(axis.text = element_text(size = 8),
        axis.text.x = element_text(angle = 90))

