
library(dplyr)
library(ggvis)

# load or create the data file --------------------------------------------

DATA_FNAME <- 'water-data.RData'

if (file.exists(DATA_FNAME)) {
  load(DATA_FNAME)
} else {
  file_list <- list.files('./data')
  water_data <- data.frame()
  
  for (file in file_list) {
    load(paste0('./data/', file))   # loads a data.frame called state_date
    water_data <- rbind(water_data, state_data)
    rm(state_data)
  }
  save(water_data, file=DATA_FNAME)
  gc()
}

water_data$feet_below_surface <- -1 * water_data$feet_below_surface

# =========================================================================

# goal: find sites that observations for every year of the data set.
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

water_data %>%
  filter(site_code %in% sites_of_interest$site_code) %>%
  ggvis(~date_time, ~feet_below_surface, fill = ~factor(site_name)) %>%
  layer_points(size:=4) %>%
  group_by(site_code) %>%
  layer_smooths()

