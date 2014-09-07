# get-data.R

sink('get-data.log', split=TRUE)

require(XML)

START_YEAR <- 2010
END_YEAR <- 2014

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


fix.columns <- function(water.data) {
  water.data$date.time <- as.Date(water.data$date.time)
  
  water.data$state.code <- as.character(water.data$state.code)
  water.data[water.data$state.code=='01','state.code'] <- 'AL'
  water.data[water.data$state.code=='02','state.code'] <- 'AK'
  water.data[water.data$state.code=='04','state.code'] <- 'AZ'
  water.data[water.data$state.code=='05','state.code'] <- 'AR'
  water.data[water.data$state.code=='06','state.code'] <- 'CA'
  water.data[water.data$state.code=='07','state.code'] <- 'CZ' # canal zone
  water.data[water.data$state.code=='08','state.code'] <- 'CO'
  water.data[water.data$state.code=='09','state.code'] <- 'CT'
  water.data[water.data$state.code=='10','state.code'] <- 'DE'
  water.data[water.data$state.code=='11','state.code'] <- 'DC'
  water.data[water.data$state.code=='12','state.code'] <- 'FL'
  water.data[water.data$state.code=='13','state.code'] <- 'GA'
  water.data[water.data$state.code=='14','state.code'] <- 'GU' # old guam code
  water.data[water.data$state.code=='15','state.code'] <- 'HI'
  water.data[water.data$state.code=='16','state.code'] <- 'ID'
  water.data[water.data$state.code=='17','state.code'] <- 'IL'
  water.data[water.data$state.code=='18','state.code'] <- 'IN'
  water.data[water.data$state.code=='19','state.code'] <- 'IA'
  water.data[water.data$state.code=='20','state.code'] <- 'KS'
  water.data[water.data$state.code=='21','state.code'] <- 'KY'
  water.data[water.data$state.code=='22','state.code'] <- 'LA'
  water.data[water.data$state.code=='23','state.code'] <- 'ME'
  water.data[water.data$state.code=='24','state.code'] <- 'MD'
  water.data[water.data$state.code=='25','state.code'] <- 'MA'
  water.data[water.data$state.code=='26','state.code'] <- 'MI'
  water.data[water.data$state.code=='27','state.code'] <- 'MN'
  water.data[water.data$state.code=='28','state.code'] <- 'MS'
  water.data[water.data$state.code=='29','state.code'] <- 'MO'
  water.data[water.data$state.code=='30','state.code'] <- 'MT'
  water.data[water.data$state.code=='31','state.code'] <- 'NE'
  water.data[water.data$state.code=='32','state.code'] <- 'NV'
  water.data[water.data$state.code=='33','state.code'] <- 'NH'
  water.data[water.data$state.code=='34','state.code'] <- 'NJ'
  water.data[water.data$state.code=='35','state.code'] <- 'NM'
  water.data[water.data$state.code=='36','state.code'] <- 'NY'
  water.data[water.data$state.code=='37','state.code'] <- 'NC'
  water.data[water.data$state.code=='38','state.code'] <- 'ND'
  water.data[water.data$state.code=='39','state.code'] <- 'OH'
  water.data[water.data$state.code=='40','state.code'] <- 'OK'
  water.data[water.data$state.code=='41','state.code'] <- 'OR'
  water.data[water.data$state.code=='42','state.code'] <- 'PA'
  water.data[water.data$state.code=='43','state.code'] <- 'PR' # old code - puerto rico
  water.data[water.data$state.code=='44','state.code'] <- 'RI'
  water.data[water.data$state.code=='45','state.code'] <- 'SC'
  water.data[water.data$state.code=='46','state.code'] <- 'SD'
  water.data[water.data$state.code=='47','state.code'] <- 'TN'
  water.data[water.data$state.code=='48','state.code'] <- 'TX'
  water.data[water.data$state.code=='49','state.code'] <- 'UT'
  water.data[water.data$state.code=='50','state.code'] <- 'VT'
  water.data[water.data$state.code=='51','state.code'] <- 'VA'
  water.data[water.data$state.code=='52','state.code'] <- 'VI' # old code - virgin islands
  water.data[water.data$state.code=='53','state.code'] <- 'VA'
  water.data[water.data$state.code=='54','state.code'] <- 'WV'
  water.data[water.data$state.code=='55','state.code'] <- 'WI'
  water.data[water.data$state.code=='56','state.code'] <- 'WY'
  water.data$state.code <- as.factor(water.data$state.code)
  
  # read value from XML wrong. oops?
  water.data$value <- as.numeric(as.character(water.data$value))
  water.data$site.latitude <- as.numeric(as.character(water.data$site.latitude))
  water.data$site.longitude <- as.numeric(as.character(water.data$site.longitude))
  
  row.names(water.data) <- 1:nrow(water.data)
  
  water.data$value[water.data$value == -999999] <- NA
  colnames(water.data)[which(colnames(water.data) == 'value')] <- 'feet.below.surface'
  
  water.data
}

process.state <- function(state.code, start.date, end.date) {
  fname <- paste0('data/', state.code, ' ', start.date, ' ', end.date, '.RData')
  
  if (file.exists(fname)) return()
  
  doc <- getWaterDoc(state=state.code, start.date=start.date, end.date=end.date)
  state.data <- parseWaterDoc(doc)
  if (nrow(state.data) > 0) {
    state.data <- fix.columns(state.data)
    save(state.data, file=fname)

    cat(paste0('Saved ', nrow(state.data), ' rows to ', fname, '\n'))
  }
}

#--------------------------------------------------------------


# request.states <- c('NH','VT','MA','ME')
# request.states <- c('ME','NH','VT','MA','RI','CT','NY','NJ',
#                     'PA','DE','MD','WV','VA','NC','SC','GA','FL')
request.states <- c('AL','AK','AZ','AR','CA',
                    'CO','CT','DE','DC','FL',
                    'GA','HI','ID','IL','IN',
                    'IA','KS','KY','LA','MD',
                    'ME','MA','MN','MS','MO',
                    'MT','NE','NE','NV','NH',
                    'NJ','NM','NY','NC','ND',
                    'OH','OK','OR','PA','RI',
                    'SC','SD','TN','TX','UT',
                    'VT','VA','WA','WV','WI',
                    'WY')

for (state in request.states) {
  for (yr in START_YEAR:END_YEAR) {
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

sink()

