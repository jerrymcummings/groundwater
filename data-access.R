# data-access.R

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
  cat(obs.url)
  
  doc <- xmlTreeParse(obs.url, getDTD=FALSE, useInternalNodes=TRUE)
  doc <- xmlRoot(doc)
}


# hackParseWaterXml
#
# given a WaterML xml string, parse it 
hackParseWaterDoc <- function(doc) {
  ns <- xmlNamespaceDefinitions(doc, simplify=TRUE)  
  timeSeries <- xpathApply(doc, "//ns1:timeSeries", namespaces=ns)
  
  for (i in seq_along(timeSeries)) {
    
    chunk <- xmlDoc(timeSeries[[i]])
    chunk <- xmlRoot(chunk)
    chunk.ns <- xmlNamespaceDefinitions(chunk, simplify=TRUE)
    
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
      
      method.id <- as.character(xpathSApply(sub.chunk, "ns1:method/@methodID", namespaces=chunk.ns))
      
      value <- as.numeric(xpathSApply(sub.chunk, 
                                      "ns1:value",
                                      namespaces=chunk.ns,
                                      xmlValue))  
      date.time <- strptime(xpathSApply(sub.chunk, 
                                        "ns1:value/@dateTime",
                                        namespaces = chunk.ns),
                            "%Y-%m-%dT%H:%M:%S")
      tz.hours <- substr(xpathSApply(sub.chunk, 
                                     "ns1:value/@dateTime",
                                     namespaces = chunk.ns),
                         23,
                         nchar(xpathSApply(sub.chunk, 
                                           "ns1:value/@dateTime",
                                           namespaces = chunk.ns)))
      #if (mean(nchar(tz.hours), rm.na=TRUE) == 6) {
      #  tz.abbrev <- zone.abbrievs[tz.hours]
      #} else {
      #  tz.abbriev <- rep(as.character(zoneAbbrievs[1]), length(date.time))
      #}
      
      qualifier <- as.character(xpathSApply(sub.chunk,
                                            "ns1:value/@qualifiers",
                                            namespaces = chunk.ns))
    }
  }  
}


#
# getSiteNames
#
# parse the xml doc for all the site names
getSiteNames <- function(water.doc) {
  ns <- xmlNamespaceDefinitions(water.doc, simplify=TRUE)  
  xpathSApply(water.doc, "//ns1:siteName", namespaces=ns, xmlValue)
}


#
# getSiteCodes
#
# parse the xml doc for all the site codes
getSiteCodes <- function(water.doc) {
  ns <- xmlNamespaceDefinitions(water.doc, simplify=TRUE)  
  xpathSApply(water.doc, "//ns1:siteCode", namespaces=ns, xmlValue)
}


#
# getSiteLocation
#
# return the lat/lon of the site
getSiteCoords <- function(water.doc, site.name=NULL, site.code=NULL) {
  ns <- xmlNamespaceDefinitions(water.doc, simplify=TRUE)
  
  core.find <- paste0("//ns1:timeSeries[ns1:sourceInfo/ns1:siteCode='",
                    site.code,
                    "']/./ns1:sourceInfo/ns1:geoLocation/ns1:geogLocation")

  result <- xpathApply(water.doc, paste0(core.find, "/ns1:latitude"), xmlValue, namespaces=ns)
  lat <- result[[1]]
  
  result <- xpathApply(water.doc, paste0(core.find, "/ns1:longitude"), xmlValue, namespaces=ns)
  lon <- result[[1]]
  
  c(latitude=lat, longitude = lon)
}

# dev test code -----------------------------------------------------------

water.doc <- getWaterDoc(start.date='2014-01-01',
                         end.date='2014-08-02',
                         state.code='NH')

site.names <- getSiteNames(water.doc)
site.codes <- getSiteCodes(water.doc)

cat(getSiteCoords(water.doc, site.code=site.codes[1]))
