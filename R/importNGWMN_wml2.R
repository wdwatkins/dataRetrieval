#' Function to return data from the National Ground Water Monitoring Network waterML2 format
#'
#' This function accepts a url parameter for a WaterML2 getObservation. This function is still under development,
#' but the general functionality is correct.
#' 
#' @param input character or raw, containing the url for the retrieval or a path to the data file, or raw XML.
#' @param asDateTime logical, if \code{TRUE} returns date and time as POSIXct, if \code{FALSE}, character
#' @param tz character to set timezone attribute of dateTime. Default is "UTC", and converts the 
#' date times to UTC, properly accounting for daylight savings times based on the data's provided time zone offset.
#' Possible values to provide are "America/New_York","America/Chicago", "America/Denver","America/Los_Angeles",
#' "America/Anchorage", as well as the following which do not use daylight savings time: "America/Honolulu",
#' "America/Jamaica","America/Managua","America/Phoenix", and "America/Metlakatla". See also  \code{OlsonNames()} 
#' for more information on time zones.
#' @return mergedDF a data frame source, time, value, uom, uomTitle, comment, gmlID
#' @export
#' @importFrom xml2 read_xml
#' @importFrom xml2 xml_find_all
#' @importFrom xml2 xml_text
#' @importFrom xml2 xml_attr
#' @importFrom xml2 xml_find_first
#' @importFrom lubridate parse_date_time
#' @examples
#' \dontrun{
#' obs_url <- paste("http://cida.usgs.gov/ngwmn_cache/sos?request=GetObservation",
#' "service=SOS","version=2.0.0",
#' "observedProperty=urn:ogc:def:property:OGC:GroundWaterLevel",
#' "responseFormat=text/xml",
#' "featureOfInterest=VW_GWDP_GEOSERVER.USGS.403836085374401",sep="&")
#' data <- importNGWMN(obs_url)
#' 
#' obs_url <- paste("http://cida.usgs.gov/ngwmn_cache/sos?request=GetObservation",
#' "service=SOS","version=2.0.0",
#' "observedProperty=urn:ogc:def:property:OGC:GroundWaterLevel",
#' "responseFormat=text/xml",
#' "featureOfInterest=VW_GWDP_GEOSERVER.USGS.474011117072901",sep="&")
#' data <- importNGWMN(obs_url)
#' }
#' 
importWaterML2 <- function(input, asDateTime=FALSE, tz="UTC"){
  
  if(tz != ""){
    tz <- match.arg(tz, OlsonNames())
  }else{tz = "UTC"}
  
  raw <- FALSE
  if(class(input) == "character" && file.exists(input)){
    returnedDoc <- read_xml(input)
  }else if(class(input) == 'raw'){
    returnedDoc <- read_xml(input)
    raw <- TRUE
  } else {
    returnedDoc <- getWebServiceData(input, encoding='gzip')
    
    returnedDoc <- xml_root(returnedDoc)
  }
  
  response <- xml_name(returnedDoc)
  if(response == "GetObservationResponse" || response == "Collection"){
    
    timeSeries <- xml_find_all(returnedDoc, "//om:OM_Observation") #each parameter/site combo
    
    if(0 == length(timeSeries)){
      df <- data.frame()
      if(!raw){
        attr(df, "url") <- input
      }
      return(df)
    }
    
    #need to treat NWIS wml2 a bit differently
    if(response == "Collection") nwis <- TRUE else nwis <- FALSE
    
    site_df_list <- lapply(X = timeSeries, FUN = parseGetObservationResponse,
                          asDateTime=asDateTime, tz = tz, nwis = nwis)
    # site_df_list <- list()
    # for(ts in timeSeries) {
    #   #this is very slow, not sure if lapply vs looping makes a difference
    #   #TODO: can importWaterML2 be optimized, or at least put a progress bar here
    #   parsed_ts <- importWaterML2(ts, asDateTime = asDateTime, tz = tz)
    #   site_df_list <- list(site_df_list, parsed_ts)
    # }
    
    #pull out attributes from each, then bind rows
    attrs_to_save <- c("dateStamp", "featureOfInterest", "contact",
                       "responsibleParty")
    attr_list <- lapply(X = site_df_list, FUN = saveAttrs, attrs = attrs_to_save)
    attr_df <- do.call(bind_rows, args = attr_list)
    mergedDF <- do.call(bind_rows, args = site_df_list)
    attributes(mergedDF) <- c(attributes(mergedDF), attr_df)
    
    if(!raw){
      url <- input
      attr(mergedDF, "url") <- url
    }
    if(!is.null(mergedDF$date) && asDateTime){
      mergedDF$date <- as.Date(mergedDF$date)
    }
    
<<<<<<< HEAD
  } else if(response == "GetFeatureOfInterestResponse"){
=======
  } else if (response == "GetFeatureOfInterestResponse"){
>>>>>>> aed66956cdfee4c33652ac467824738540b4260b
    featureMembers <- xml_find_all(returnedDoc, ".//sos:featureMember")
    site <- xml_text(xml_find_all(featureMembers,".//gml:identifier"))
    site <- substring(site, 8)
    
    #some sites don't have a description
    siteDesc <- xml_text(xml_find_first(featureMembers, ".//gml:description"))
    
    siteLocs <- strsplit(xml_text(xml_find_all(featureMembers, ".//gml:pos")), " ")
    siteLocs <- data.frame(matrix(unlist(siteLocs), nrow=length(siteLocs), byrow=TRUE), stringsAsFactors = FALSE)
    names(siteLocs) <- c("dec_lat_va", "dec_lon_va")
    dec_lat_va <- "dplyr var"
    dec_lon_va <- "dplyr var"
    siteLocs <- mutate(siteLocs, dec_lat_va=as.numeric(dec_lat_va), dec_lon_va=as.numeric(dec_lon_va))
    mergedDF <- cbind.data.frame(site, description = siteDesc, siteLocs, stringsAsFactors = FALSE) 
  
  } else if (response == "ExceptionReport"){
    return(data.frame())
  } else {
    stop("Unrecognized response from the web service")
    return(data.frame())
  }
  return(mergedDF)
}


#' Read waterML2 inside a SOS observation block 
#'
#' @param input input nodes - should be an sos:observationData node and children
#' @param asDateTime logical should dateTimes be converted from strings?
#' @param tz character timezone
#' @param nwis logical Is this from NWIS or NGWMN
#' @return parsed ML2 data and associated metadata
#' @importFrom xml2 xml_find_all xml_text xml_attr
#' @importFrom dplyr mutate select
#'
parseGetObservationResponse <- function(input, asDateTime, tz, nwis){
  wml2_nodes <- xml_find_all(input, ".//wml2:MeasurementTimeseries")
  if(length(wml2_nodes) == 0) { #site has no data
    emptyDF <- data.frame()
    #if these attributes are nulls it breaks bind_rows later
    attr(emptyDF, "contact") <- NA
    attr(emptyDF, "dateStamp") <- NA
    attr(emptyDF, "featureOfInterest") <- NA
    attr(emptyDF, "responsibleParty") <- NA
    return(emptyDF)
  }
  wml2_parsed <- parseWaterML2TimeSeries(wml2_nodes, asDateTime, tz)
  message("finished one")
  foi <- xml_attr(xml_find_all(input, ".//om:featureOfInterest"), "title") 
  foi_agency_id <- sub('.*\\ ', '', foi)
  siteID <- tail(strsplit(x = foi_agency_id, split = "[.|-]")[[1]], n = 1)
  #add new columns
  if(!nwis) {
    wml2_parsed <- mutate(wml2_parsed, featureOfInterest=foi_agency_id, site_no=siteID)
    wml2_parsed <- select(wml2_parsed, featureOfInterest, source, 
                          site_no, everything())
    site_no <- '.dplyr_var'
    featureOfInterest <- '.dply_var'
    #tack on attributes
    attr(wml2_parsed, "dateStamp") <- xml_text(xml_find_all(input, ".//gco:DateTime")) 
    attr(wml2_parsed, "featureOfInterest") <- foi_agency_id
    meta <- xml_find_all(input, ".//gmd:contact")
    attr(wml2_parsed, "contact") <- xml_attr(meta, "href")
    attr(wml2_parsed, "responsibleParty") <- xml_text(xml_find_all(meta, ".//gco:CharacterString"))
  }
  
  return(wml2_parsed)
}


#' Parse the WaterML2 timeseries portion of a waterML2 file
#' 
#' Returns data frame columns of all information with each time series measurement;
#' Anything defined as a default, is returned as an attribute of that data frame.
#' 
#' @param input XML with only the wml2:MeasurementTimeseries node and children
#' @param asDateTime logical, if \code{TRUE} returns date and time as POSIXct, if \code{FALSE}, character
#' @param tz character to set timezone attribute of datetime. Default is an empty quote, which converts the 
#' datetimes to UTC (properly accounting for daylight savings times based on the data's provided time zone offset).
#' Possible values are "America/New_York","America/Chicago", "America/Denver","America/Los_Angeles",
#' "America/Anchorage","America/Honolulu","America/Jamaica","America/Managua","America/Phoenix", and "America/Metlakatla"
#' @importFrom xml2 xml_attr xml_find_all xml_text 
#' @importFrom dplyr mutate
#' @importFrom lubridate parse_date_time
parseWaterML2TimeSeries <- function(input, asDateTime, tz) {
  
  gmlID <- xml_attr(input,"id") #TODO: make this an attribute
  TVP <- xml_find_all(input, ".//wml2:MeasurementTVP")#time-value pairs
  if(length(TVP) == 0) { #empty nodes on some sites
    return(data.frame(site = character(0), source = character(0), date = character(0),
                      time = character(0), dateTime = character(0), value = numeric(0),
                      uom = character(0), comment = character(0), stringsAsFactors = FALSE))
  }

  rawTime <- xml_text(xml_find_all(returnedDoc, "./wml2:point/wml2:MeasurementTVP/wml2:time"))
  
  valueNodes <- xml_find_all(returnedDoc,"./wml2:point/wml2:MeasurementTVP/wml2:value")
  charValues <- xml_text(valueNodes)
  nilValues <- as.logical(xml_attr(valueNodes, "nil"))
  charValues[nilValues] <- NA
  values <- as.numeric(charValues)
  nVals <- length(values)
  
  #df of date, time, dateTime
  oneCol <- rep(NA, nVals) 
  timeDF <- data.frame(date=oneCol, time=oneCol, dateTime=oneCol)
  splitTime <- data.frame(matrix(unlist(strsplit(rawTime, "T")), nrow=nVals, byrow = TRUE), stringsAsFactors=FALSE)
  if(ncol(splitTime) > 1){ #some sites only have a date
    names(splitTime) <- c("date", "time")
  }else{
    names(splitTime) <- "date"
    splitTime <- mutate(splitTime, time = NA)
  }
  
  timeDF <- mutate(splitTime, dateTime = NA)
  logicVec <- nchar(rawTime) > 19
  if(!all(!logicVec)) { #otherwise sets it to char <NA>
    timeDF$dateTime[logicVec] <- rawTime[logicVec]
  }
  if(asDateTime){
    timeDF$dateTime <- parse_date_time(timeDF$dateTime, c("%Y","%Y-%m-%d","%Y-%m-%dT%H:%M","%Y-%m-%dT%H:%M:%S",
                                                          "%Y-%m-%dT%H:%M:%OS","%Y-%m-%dT%H:%M:%OS%z"), exact = TRUE)
    #^^setting tz in as.POSIXct just sets the attribute, does not convert the time!
    attr(timeDF$dateTime, 'tzone') <- tz
  }
  
  uom <- xml_attr(valueNodes, "uom", default = NA)

  source <- xml_attr(xml_find_all(returnedDoc, 
                                  "./wml2:point/wml2:MeasurementTVP/wml2:metadata/wml2:source"), 
                     "title")
  comment <- xml_text(xml_find_all(returnedDoc, 
                  "./wml2:point/wml2:MeasurementTVP/wml2:metadata/wml2:comment"))
  tvpQuals <- xml_text(xml_find_all(returnedDoc, 
                        "./wml2:point/wml2:MeasurementTVP/wml2:metadata/swe:description"))
  defaultMeta <- xml_find_all(returnedDoc, ".//wml2:DefaultTVPMeasurementMetadata")
  defaultQuals <- xml_text(xml_find_all(defaultMeta, ".//swe:description"))
  defaultUOM <- xml_attr(xml_find_all(defaultMeta, ".//wml2:uom"), "title", default = NA)
 
  df_vars <- list(source = source, timeDF, value = values, 
                  uom = uom, comment = comment)
  df_use <- df_vars[sapply(df_vars, function(x){length(x) > 0 && !all(is.na(x))})]
  df <- data.frame(df_use, stringsAsFactors = FALSE)
  if(!"value" %in% names(df)) {df$value <- NA_real_}
  #from the default metadata section
  #append to existing attributes if they aren't empty
   mdAttribs <- list(defaultQualifier=defaultQuals, defaultUOM=defaultUOM, 
                    gmlID=gmlID) #all attributes must have names
  mdAttribs_use <- mdAttribs[sapply(mdAttribs, function(x){length(x) > 0})]
  attributes(df) <- append(attributes(df), mdAttribs_use)
  return(df)
}