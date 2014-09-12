# exploratory-data-analysis.R


# doy <- function(dt) {
#   as.integer(strftime(dt, format="%j"))
# }
# water.data$day.of.year <- doy(water.data$date.time)
# 
# water.data.df <- tbl_df(water.data)
# 
# site.with.most.obs <-
#   water.data.df %>%
#   group_by(site.code) %>%
#   summarize(obs.counts=n()) %>%
#   arrange(desc(obs.counts)) %>%
#   head(1)
# 
# one.site <- water.data.df %>%
#   filter(site.code == site.with.most.obs$site.code)
# 
# # scatter plot of the data for this site
# ggplot(one.site, aes(x=date.time, y=feet.below.surface)) +
#   geom_point(size=1.5, color='darkgray') +
#   geom_smooth(method='lm') +
#   ggtitle(one.site$site.name) +
#   theme_bw()
# 
# # look for site with most history
# site.with.most.history <-
#   water.data.df %>%
#   group_by(site.code) %>%
#   summarize(date.range=as.numeric(max(date.time) - min(date.time))) %>%
#   arrange(desc(date.range)) %>%
#   head(1)
# 
# one.site <- water.data.df %>%
#   filter(site.code == site.with.most.history$site.code)

# debugging
#one.site <- one.site %>%
#  filter(date.time >= '1980-01-01' 
#         & date.time <= '1991-01-01')

# ggplot(one.site, aes(x=date.time, y=feet.below.surface)) +
#   geom_point(size=1.5, color='darkgray') +
#   geom_line() +
#   #geom_smooth(method='lm') +
#   ggtitle(one.site$site.name)
# 
# # get date.time of greatest value for each year.
# max.values <-
#   one.site %>%
#   mutate(obs.year=as.integer(format(date.time, "%Y"))) %>%
#   group_by(obs.year) %>%
#   filter(feet.below.surface == max(feet.below.surface))
# 
# max.values$date.time
# 
# max.data <-
#   water.data.df %>%
#   mutate(obs.year=as.integer(format(date.time, '%Y'))) %>%
#   group_by(site.code, obs.year) %>%
#   filter(feet.below.surface == max(feet.below.surface) &
#            obs.year >= 2010 & obs.year <= 2013) %>%
#   group_by(site.code, site.latitude, site.longitude) %>%
#   summarize(median.day.of.year.for.max=median(day.of.year))
# 
# map <- get_map(location="Richmond, Virginia", zoom=5)
# 
# ggmap(map) +
#   geom_point(data=max.data,
#              aes(x=site.longitude,
#                  y=site.latitude,
#                  color=median.day.of.year.for.max)) +
#   scale_color_gradientn(colours=rainbow(356))
# 

# 
# data.years <- filter(water.data.df, date.time >= '1900-01-01')
# data.years <- filter(data.years, value != -999999)
# data.years <- arrange(data.years, site.code, date.time)
# data.years <- group_by(data.years, site.code)
# 
# means.years <- summarize(data.years,
#                          site.latitude = max(site.latitude),
#                          site.longitude = max(site.longitude),
#                          obs.count=n(),
#                          obs.mean.value=mean(value))
# 
# # just New Hampshire
# nh.data.years <- filter(data.years, state.code == 'NH')
# 
# # which ones have only a few data points? We'd like to see
# # twelve if one per month. Too few and we should drop them
# counts.nh.years <- summarize(nh.data.years, obs.count=n())
# ok.on.counts <- filter(counts.nh.years, obs.count > 2)
# nh.data.years <- filter(nh.data.years, site.code %in% ok.on.counts$site.code)
# 
# ggplot(nh.data.years, aes(x=date.time, y=value)) +
#   geom_point() +
#   facet_wrap(~site.code) +
#   theme_bw()
# 
# # visually interesting:
# nh.oddball.sites <- c(
#   '435558071405820',
#   '424552071154901')
# 
# nh.typical.sites <- c(
#   '425406070592601',
#   '425407070592801'
# )
# 
# 
# p <- ggplot(subset(nh.data.years, 
#                    site.code %in% c(nh.oddball.sites, nh.typical.sites)),
#             aes(x=date.time, y=value)) +
#   
#   geom_point() +
#   geom_line() +
#   
#   #geom_vline(xintercept=as.numeric(as.Date('2012-01-01')), color='lightblue') +
#   #geom_vline(xintercept=as.numeric(as.Date('2013-01-01')), color='lightblue') +
#   
#   facet_wrap(~site.code) +
#   theme_bw()
# print (p)
# 
# if (FALSE) {
#   map <- get_map(location='Loudon, New Hampshire', zoom=7)
#   ggmap(map) +
#     geom_point(data=means.years,
#                aes(x=site.longitude,
#                    y=site.latitude,
#                    color=obs.mean.value))
#   
# }

library(dplyr)
library(ggplot2)
library(ggmap)
library(RColorBrewer)
library(animation)

DATA_FNAME <- 'water.data.RData'

if (file.exists(DATA_FNAME)) {
  load(DATA_FNAME)
} else {
  file.list <- list.files('./data')
  water.data <- data.frame()
  
  for (file in file.list) {
    load(paste0('./data/', file))
    water.data <- rbind(water.data, state.data) # state.data is what we loaded
    rm(state.data)
  }
  save(water.data, file=DATA_FNAME)
}

water.data.df <- tbl_df(water.data)

if (FALSE) {
  # number of observations per state
  table(water.data$state.code)
}

if (TRUE) {
  NBR_SAMPLES <- 3
  
  sites <- group_by(water.data.df, site.code)
  site.obs.counts <- summarize(sites, count=n())
  summary(site.obs.counts$count)
  ok.sites <- site.obs.counts %>% filter(count > 200)
  
  ok.data <- 
    water.data.df %>%
    filter(site.code %in% ok.sites)
  
  idx <- sample(1:nrow(ok.sites), size=NBR_SAMPLES, replace=FALSE)
  sample.site.codes <- ok.sites$site.code[idx]
  sample.data <-
    water.data.df %>%
    filter(site.code %in% sample.site.codes)
  
  ggplot(sample.data,
         aes(x=date.time,
             y=feet.below.surface,
             color=site.name)) +
    geom_point() +
    theme_bw()

  sample.min.max <-
    sample.data %>%
    group_by(site.code) %>%
    summarize(min.feet.below.surface=min(feet.below.surface),
              max.feet.below.surface=max(feet.below.surface))
  
  sample.data <-
    tbl_df(merge(sample.data, sample.min.max, by='site.code'))

  sample.data$normalized.value <- NULL
  sample.data <-
    mutate(sample.data,
           normalized.value =
             (feet.below.surface - min.feet.below.surface) /
             (max.feet.below.surface - min.feet.below.surface))

  ggplot(sample.data,
         aes(x=date.time,
             y=normalized.value,
             color=site.name)) +
    geom_point() +
    theme_bw()
}


if (FALSE) {
  # get sites that have at least a year's
  # worth of data
  ok.sites <-
    water.data.df %>%
    group_by(site.code) %>%
    summarize(date.range=as.numeric(max(date.time) - min(date.time))) %>%
    filter(date.range >= 365)
  
  # get the water data for those sites
  ok.data <-
    water.data.df %>%
    filter(site.code %in% ok.sites$site.code) %>%
    filter(feet.below.surface <= 3000.0)
  
  # determine min and max value of each site
  ok.min.max <-
    ok.data %>%
    group_by(site.code) %>%
    summarize(min.feet.below.surface=min(feet.below.surface),
              max.feet.below.surface=max(feet.below.surface))
  
  ok.data <-
    tbl_df(merge(ok.data, ok.min.max, by='site.code'))
  
  ok.data$normalized.value <- NULL
  ok.data <-
    mutate(ok.data,
           normalized.value =
             (feet.below.surface - min.feet.below.surface) /
             (max.feet.below.surface - min.feet.below.surface))
  
  # determine center of the map that we need
  #
  # http://en.wikipedia.org/wiki/Geographic_center_of_the_contiguous_United_States
  center.lat = 39 + 50/60
  center.lon = -(98 + 35/60)
  
  # hmmm.... I don't like theirs. A little bitty
  # tweak to see if we can keep zoom=4 and get most
  # data points
  center.lat = 39.83
  center.lon = -96.50
  
  map <- get_map(location=c(lon=center.lon,
                            lat=center.lat),
                 zoom=4)
  
  numberOfDaysInMonth <- function(date) {
    m <- format(date, format="%m")
    
    while (format(date, format="%m") == m) {
      date <- date + 1
    }
    
    return(as.integer(format(date - 1, format="%d")))
  }
  
  # TODO:
  #
  # figure out how to chunk up the data by date
  # so that we can draw a sequence of plots showing
  # water level changes over time
  START_YEAR <- 2010
  END_YEAR <- 2014
  OUTPUT_FNAME <- 'groundwater.gif'
  
  ani.options(outdir = getwd())
  
  # need to figure out how to create a
  # rolling data set
  
  if (file.exists(OUTPUT_FNAME)) {
    file.remove(OUTPUT_FNAME)
  }
  
  saveGIF({
    for (yr in START_YEAR:END_YEAR) {
      for (mo in 1:12) {
        day.1 = 1
        
        dt.1 <- as.Date(sprintf('%d-%s-%s',
                                yr,
                                formatC(mo, width=2, format='d', flag='0'),
                                formatC(day.1, width=2, format='d', flag='0')))
        
        dt.2 <- (seq(dt.1, length=2, by="months")-1)[2]
        
        month.data <-
          ok.data %>%
          filter(date.time >= dt.1 & date.time <= dt.2)
        
        if (nrow(month.data) > 0) {
          month.agg <-
            month.data %>%
            group_by(site.code, site.longitude, site.latitude) %>%
            summarize(median.normalized.value=median(normalized.value))
          
          p <- ggmap(map) +
            geom_point(data=month.agg,
                       aes(x=site.longitude,
                           y=site.latitude,
                           color=median.normalized.value)) +
            geom_text(aes(x=-120.0, y=20.0,
                          label=paste0(month.name[mo], ' ', yr),
                          size=20))
          
          print(p)
        }
      }
    }
  },
  movie.name=OUTPUT_FNAME,
  interval = 0.5,
  ani.width = 1024, 
  ani.height = 768
  )
}
