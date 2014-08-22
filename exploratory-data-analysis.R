# exploratory-data-analysis.R

library(dplyr)
#library(ggvis)
library(ggplot2)
library(ggmap)


load('water.data.Rdata')

water.data.df <- tbl_df(water.data)

colnames(water.data.df)

data.years <- filter(water.data.df, format(date.time, '%Y') %in% c('2011','2012','2013'))
data.years <- filter(data.years, value != -999999)
data.years <- arrange(data.years, site.code, date.time)
data.years <- group_by(data.years, site.code)

means.years <- summarize(data.years,
                        site.latitude = max(site.latitude),
                        site.longitude = max(site.longitude),
                        obs.count=n(),
                        obs.mean.value=mean(value))

# just New Hampshire
nh.data.years <- filter(data.years, state.code == 'NH')

# which ones have only a few data points? We'd like to see
# twelve if one per month. Too few and we should drop them
counts.nh.years <- summarize(nh.data.years, obs.count=n())
ok.on.counts <- filter(counts.nh.years, obs.count > 2)
nh.data.years <- filter(nh.data.years, site.code %in% ok.on.counts$site.code)

ggplot(nh.data.years, aes(x=date.time, y=value)) +
  geom_point() +
  facet_wrap(~site.code) +
  theme_bw()

# visually interesting:
nh.oddball.sites <- c(
  '435558071405820',
  '424552071154901')

nh.typical.sites <- c(
  '425406070592601',
  '425407070592801'
  )

ggplot(subset(nh.data.years, site.code %in% nh.oddball.sites),
       aes(x=date.time, y=value)) +

  geom_point() +
  geom_line() +
  
  geom_vline(xintercept=as.numeric(as.Date('2012-01-01')), color='lightblue') +
  geom_vline(xintercept=as.numeric(as.Date('2013-01-01')), color='lightblue') +

  facet_wrap(~site.code) +
  theme_bw()

ggplot(subset(nh.data.years, site.code %in% nh.typical.sites),
       aes(x=date.time, y=value)) +
  
  geom_point() +
  geom_line() +
  
  geom_vline(xintercept=as.numeric(as.Date('2012-01-01')), color='lightblue') +
  geom_vline(xintercept=as.numeric(as.Date('2013-01-01')), color='lightblue') +

  facet_wrap(~site.code) +
  theme_bw()

if (FALSE) {
  map <- get_map(location='Loudon, New Hampshire', zoom=7)
  ggmap(map) +
    geom_point(data=means.years,
               aes(x=site.longitude,
                   y=site.latitude,
                   color=obs.mean.value))
  
}
