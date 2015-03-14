
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

# =========================================================================

water_data %>%
  filter(site_code %in% c(430527071140101, 445334071291701)) %>%
  ggvis(~date_time, ~feet_below_surface, fill = ~factor(site_name)) %>%
  layer_points()

