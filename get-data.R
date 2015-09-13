# get-data.R

sink('get-data.log', split=TRUE)

require(XML)

START_YEAR <- 1960
END_YEAR <- 2015

request_states <- c('NH')
# request_states <- c('ME','NH','VT','MA','RI','CT','NY','NJ',
#                     'PA','DE','MD','WV','VA','NC','SC','GA','FL')
# request_states <- c('AL','AK','AZ','AR','CA',
#                     'CO','CT','DE','DC','FL',
#                     'GA','HI','ID','IL','IN',
#                     'IA','KS','KY','LA','MD',
#                     'ME','MA','MN','MS','MO',
#                     'MT','NE','NE','NV','NH',
#                     'NJ','NM','NY','NC','ND',
#                     'OH','OK','OR','PA','RI',
#                     'SC','SD','TN','TX','UT',
#                     'VT','VA','WA','WV','WI',
#                     'WY')

# =====================================================================

wlog <- function(s) {
  cat(paste0(Sys.time(), ' ', s, '\n'))
}

# =====================================================================

#
# get_water_doc
#
# given the args, form the requisite URL and fetch the XML
get_water_doc <- function(start_date,
                          end_date,
                          state_code) {
  
  obs_url <- paste0("http://waterservices.usgs.gov/nwis/gwlevels/?format=waterml",
                    "&stateCd=", state_code, 
                    "&startDT=", start_date,
                    "&endDT=", end_date)
  
  doc <- xmlTreeParse(obs_url, getDTD=FALSE, useInternalNodes=TRUE)
  doc <- xmlRoot(doc)
}


# given a WaterML xml string, parse it 
parse_water_doc <- function(doc) {
  df <- data.frame()
  
  ns <- xmlNamespaceDefinitions(doc, simplify=TRUE)  
  
  time_series <- xpathApply(doc, "//ns1:timeSeries", namespaces=ns)
  
  for (i in seq_along(time_series)) {
    
    chunk <- xmlDoc(time_series[[i]])
    chunk <- xmlRoot(chunk)
    chunk_ns <- xmlNamespaceDefinitions(chunk, simplify=TRUE)
    
    state_code <- as.character(
      xpathApply(chunk,
                 "//ns1:sourceInfo/ns1:siteProperty[@name='stateCd']",
                 xmlValue, 
                 namespaces=chunk_ns))
    
    site_name <- as.character(
      xpathApply(chunk,
                 "ns1:sourceInfo/ns1:siteName",
                 namespaces=chunk_ns,
                 xmlValue))
    site_code <- as.character(
      xpathApply(chunk,
                 "ns1:sourceInfo/ns1:siteCode", 
                 namespaces=chunk_ns, 
                 xmlValue))
    
    site_latitude <- as.numeric(
      xpathApply(chunk, 
                 "ns1:sourceInfo/ns1:geoLocation/ns1:geogLocation/ns1:latitude", 
                 namespaces=chunk_ns, 
                 xmlValue))
    site_longitude <- as.numeric(
      xpathApply(chunk, 
                 "ns1:sourceInfo/ns1:geoLocation/ns1:geogLocation/ns1:longitude", 
                 namespaces=chunk_ns,
                 xmlValue))
    
    values_index <- as.numeric(which(names(chunk) == 'values'))
    for (j in values_index) {
      sub_chunk <- xmlRoot(xmlDoc(chunk[[j]]))
      
      # depth to water level, feet below land surface
      values <- as.numeric(
        xpathSApply(sub_chunk, 
                    "ns1:value",
                    namespaces=chunk_ns,
                    xmlValue))  
      date_times <- xpathSApply(sub_chunk, 
                                "ns1:value/@dateTime",
                                namespaces = chunk_ns)
      
      one_set <- cbind(
        state_code=rep(state_code, length(values)),
        site_name=rep(site_name, length(values)),
        site_code=rep(site_code, length(values)),
        site_latitude=rep(site_latitude, length(values)),
        site_longitude=rep(site_longitude, length(values)),
        value=values,
        date_time=date_times
      )
      
      
      df <- rbind(df, one_set)
    }
  }  
  
  df
}


# fix_columns -------------------------------------------------------------

fix_columns <- function(water_data) {
  water_data$date_time <- as.Date(water_data$date_time)
  
  water_data$state_code <- as.character(water_data$state_code)
  water_data[water_data$state_code=='01','state_code'] <- 'AL'
  water_data[water_data$state_code=='02','state_code'] <- 'AK'
  water_data[water_data$state_code=='04','state_code'] <- 'AZ'
  water_data[water_data$state_code=='05','state_code'] <- 'AR'
  water_data[water_data$state_code=='06','state_code'] <- 'CA'
  water_data[water_data$state_code=='07','state_code'] <- 'CZ' # canal zone
  water_data[water_data$state_code=='08','state_code'] <- 'CO'
  water_data[water_data$state_code=='09','state_code'] <- 'CT'
  water_data[water_data$state_code=='10','state_code'] <- 'DE'
  water_data[water_data$state_code=='11','state_code'] <- 'DC'
  water_data[water_data$state_code=='12','state_code'] <- 'FL'
  water_data[water_data$state_code=='13','state_code'] <- 'GA'
  water_data[water_data$state_code=='14','state_code'] <- 'GU' # old guam code
  water_data[water_data$state_code=='15','state_code'] <- 'HI'
  water_data[water_data$state_code=='16','state_code'] <- 'ID'
  water_data[water_data$state_code=='17','state_code'] <- 'IL'
  water_data[water_data$state_code=='18','state_code'] <- 'IN'
  water_data[water_data$state_code=='19','state_code'] <- 'IA'
  water_data[water_data$state_code=='20','state_code'] <- 'KS'
  water_data[water_data$state_code=='21','state_code'] <- 'KY'
  water_data[water_data$state_code=='22','state_code'] <- 'LA'
  water_data[water_data$state_code=='23','state_code'] <- 'ME'
  water_data[water_data$state_code=='24','state_code'] <- 'MD'
  water_data[water_data$state_code=='25','state_code'] <- 'MA'
  water_data[water_data$state_code=='26','state_code'] <- 'MI'
  water_data[water_data$state_code=='27','state_code'] <- 'MN'
  water_data[water_data$state_code=='28','state_code'] <- 'MS'
  water_data[water_data$state_code=='29','state_code'] <- 'MO'
  water_data[water_data$state_code=='30','state_code'] <- 'MT'
  water_data[water_data$state_code=='31','state_code'] <- 'NE'
  water_data[water_data$state_code=='32','state_code'] <- 'NV'
  water_data[water_data$state_code=='33','state_code'] <- 'NH'
  water_data[water_data$state_code=='34','state_code'] <- 'NJ'
  water_data[water_data$state_code=='35','state_code'] <- 'NM'
  water_data[water_data$state_code=='36','state_code'] <- 'NY'
  water_data[water_data$state_code=='37','state_code'] <- 'NC'
  water_data[water_data$state_code=='38','state_code'] <- 'ND'
  water_data[water_data$state_code=='39','state_code'] <- 'OH'
  water_data[water_data$state_code=='40','state_code'] <- 'OK'
  water_data[water_data$state_code=='41','state_code'] <- 'OR'
  water_data[water_data$state_code=='42','state_code'] <- 'PA'
  water_data[water_data$state_code=='43','state_code'] <- 'PR' # old code - puerto rico
  water_data[water_data$state_code=='44','state_code'] <- 'RI'
  water_data[water_data$state_code=='45','state_code'] <- 'SC'
  water_data[water_data$state_code=='46','state_code'] <- 'SD'
  water_data[water_data$state_code=='47','state_code'] <- 'TN'
  water_data[water_data$state_code=='48','state_code'] <- 'TX'
  water_data[water_data$state_code=='49','state_code'] <- 'UT'
  water_data[water_data$state_code=='50','state_code'] <- 'VT'
  water_data[water_data$state_code=='51','state_code'] <- 'VA'
  water_data[water_data$state_code=='52','state_code'] <- 'VI' # old code - virgin islands
  water_data[water_data$state_code=='53','state_code'] <- 'VA'
  water_data[water_data$state_code=='54','state_code'] <- 'WV'
  water_data[water_data$state_code=='55','state_code'] <- 'WI'
  water_data[water_data$state_code=='56','state_code'] <- 'WY'
  water_data$state_code <- as.factor(water_data$state_code)
  
  # read value from XML wrong. oops?
  water_data$value <- as.numeric(as.character(water_data$value))
  water_data$site_latitude <- as.numeric(as.character(water_data$site_latitude))
  water_data$site_longitude <- as.numeric(as.character(water_data$site_longitude))
  
  row.names(water_data) <- 1:nrow(water_data)
  
  water_data$value[water_data$value == -999999] <- NA
  colnames(water_data)[which(colnames(water_data) == 'value')] <- 'feet_below_surface'
  
  water_data
}


# process_state -----------------------------------------------------------

process_state <- function(state_code, start_date, end_date) {
  if (!dir.exists('data')) {
    dir.create('data')  
  }
  
  fname <- paste0('data/', state_code, ' ', start_date, ' ', end_date, '.RData')
  
  if (file.exists(fname)) return()
  
  doc <- get_water_doc(state=state_code, start_date=start_date, end_date=end_date)
  state_data <- parse_water_doc(doc)
  if (nrow(state_data) > 0) {
    state_data <- fix_columns(state_data)
    save(state_data, file=fname)
    
    wlog(paste0('Saved ', nrow(state_data), ' rows to ', fname))
  }
}

#--------------------------------------------------------------

for (state in request_states) {
  for (yr in START_YEAR:END_YEAR) {
    for (mo in 1:12) {
      day.1 = 1
      
      dt.1 <- as.Date(sprintf('%d-%s-%s',
                              yr,
                              formatC(mo, width=2, format='d', flag='0'),
                              formatC(day.1, width=2, format='d', flag='0')))
      
      dt.2 <- (seq(dt.1, length=2, by="months")-1)[2]
      
      wlog(paste0('...doing ', state, ', ', dt.1, ' to ', dt.2))
      result <- try(process_state(state_code=state, start_date=dt.1, end_date=dt.2))
      if (class(result) == "try-error") {
        wlog(paste0('*** PROBLEM doing ', state, ', ', dt.1, ' to ', dt.2))
        next
      }
    }
  }
}

sink()



