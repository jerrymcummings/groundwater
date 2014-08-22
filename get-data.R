# get-data.R

require(XML)

#
# getWaterDoc
#
# given the args, form the requisite URL and fetch the XML
getWaterDoc <- function(start.date,
                        end.date,
                        state.code) {
  
  obs.url <- paste0("http://waterservices.usgs.gov/nwis/gwlevels/?format=waterml",
                    "&stateCd=", state.code, 
                    "&startDT=", start.date,
                    "&endDT=", end.date)
  
  doc <- xmlTreeParse(obs.url, getDTD=FALSE, useInternalNodes=TRUE)
  doc <- xmlRoot(doc)
}


# given a WaterML xml string, parse it 
parseWaterDoc <- function(doc) {
  df <- data.frame()
  
  ns <- xmlNamespaceDefinitions(doc, simplify=TRUE)  
    
  time.series <- xpathApply(doc, "//ns1:timeSeries", namespaces=ns)
  
  for (i in seq_along(time.series)) {
    
    chunk <- xmlDoc(time.series[[i]])
    chunk <- xmlRoot(chunk)
    chunk.ns <- xmlNamespaceDefinitions(chunk, simplify=TRUE)
    
    state.code <- as.character(xpathApply(chunk, "//ns1:sourceInfo/ns1:siteProperty[@name='stateCd']", xmlValue, namespaces=chunk.ns))
    
    site.name <- as.character(xpathApply(chunk,
                                         "ns1:sourceInfo/ns1:siteName",
                                         namespaces=chunk.ns,
                                         xmlValue))
    site.code <- as.character(xpathApply(chunk,
                                         "ns1:sourceInfo/ns1:siteCode", 
                                         namespaces=chunk.ns, 
                                         xmlValue))
    site.latitude <- as.numeric(xpathApply(chunk, 
                                           "ns1:sourceInfo/ns1:geoLocation/ns1:geogLocation/ns1:latitude", 
                                           namespaces=chunk.ns, 
                                           xmlValue))
    site.longitude <- as.numeric(xpathApply(chunk, 
                                            "ns1:sourceInfo/ns1:geoLocation/ns1:geogLocation/ns1:longitude", 
                                            namespaces=chunk.ns,
                                            xmlValue))
    
    values.index <- as.numeric(which(names(chunk) == 'values'))
    for (j in values.index) {
      sub.chunk <- xmlRoot(xmlDoc(chunk[[j]]))
      
      # depth to water level, feet below land surface
      values <- as.numeric(xpathSApply(sub.chunk, 
                                       "ns1:value",
                                       namespaces=chunk.ns,
                                       xmlValue))  
      date.times <- xpathSApply(sub.chunk, 
                                         "ns1:value/@dateTime",
                                         namespaces = chunk.ns)#,
                             #"%Y-%m-%dT%H:%M:%S")

      
      one.set <- cbind(
        state.code=rep(state.code, length(values)),
        site.name=rep(site.name, length(values)),
        site.code=rep(site.code, length(values)),
        site.latitude=rep(site.latitude, length(values)),
        site.longitude=rep(site.longitude, length(values)),
        value=values,
        date.time=date.times
      )
      
    
      df <- rbind(df, one.set)
    }
  }  
  
  df
}

process.state <- function(state.code, start.date, end.date) {
  doc <- getWaterDoc(state=state.code, start.date=start.date, end.date=end.date)
  state.data <- parseWaterDoc(doc)
  water.data <<- rbind(water.data, state.data)
}

#--------------------------------------------------------------


request.states <- c('NH','VT','MA','ME')

water.data <- data.frame()
for (state in request.states) {
  for (yr in 1990:1991) {
    for (mo in 1:12) {
      day.1 = 1
      
      dt.1 <- as.Date(sprintf('%d-%s-%s',
                              yr,
                              formatC(mo, width=2, format='d', flag='0'),
                              formatC(day.1, width=2, format='d', flag='0')))
      
      dt.2 <- (seq(dt.1, length=2, by="months")-1)[2]

      cat(paste0('...doing ', state, ', ', dt.1, ' to ', dt.2, '\n'))
      result <- try(process.state(state.code=state, start.date=dt.1, end.date=dt.2))
      if (class(result) == "try-error") {
        cat(paste0('*** PROBLEM doing ', state, ', ', dt.1, ' to ', dt.2, '\n'))
        next
      }
    }
  }
}

water.data$date.time <- as.Date(water.data$date.time)

water.data$state.code <- as.character(water.data$state.code)
water.data[water.data$state.code=='23','state.code'] <- 'ME'
water.data[water.data$state.code=='25','state.code'] <- 'MA'
water.data[water.data$state.code=='33','state.code'] <- 'NH'
water.data[water.data$state.code=='50','state.code'] <- 'VT'
water.data$state.code <- as.factor(water.data$state.code)

# read value from XML wrong. oops?
water.data$value <- as.numeric(as.character(water.data$value))
water.data$site.latitude <- as.numeric(as.character(water.data$site.latitude))
water.data$site.longitude <- as.numeric(as.character(water.data$site.longitude))

row.names(water.data) <- 1:nrow(water.data)

save(water.data, file='water.data.Rdata')

# make this a function! for analysis, we can save/load the dataframe
# for now.

