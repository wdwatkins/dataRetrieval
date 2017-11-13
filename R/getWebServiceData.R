#' Function to return data from web services
#'
#' This function accepts a url parameter, and returns the raw data. The function enhances
#' \code{\link[httr]{GET}} with more informative error messages.
#'
#' @param obs_url character containing the url for the retrieval
#' @param \dots information to pass to header request
#' @importFrom httr GET
#' @importFrom httr POST
#' @importFrom httr RETRY
#' @importFrom httr user_agent
#' @importFrom httr stop_for_status
#' @importFrom httr status_code
#' @importFrom httr headers
#' @importFrom httr content
#' @importFrom httr content_type
#' @importFrom curl curl_version
#' @importFrom xml2 xml_text
#' @importFrom xml2 xml_child
#' @importFrom xml2 read_xml
#' @export
#' @return raw data from web services
#' @examples
#' siteNumber <- "02177000"
#' startDate <- "2012-09-01"
#' endDate <- "2012-10-01"
#' offering <- '00003'
#' property <- '00060'
#' obs_url <- constructNWISURL(siteNumber,property,startDate,endDate,'dv')
#' \dontrun{
#' rawData <- getWebServiceData(obs_url)
#' }
getWebServiceData <- function(obs_url, ...){
  
  returnedList <- retryGetOrPost(obs_url, ...)
  
  if(status_code(returnedList) == 400){
    response400 <- content(returnedList, type="text", encoding = "UTF-8")
    statusReport <- xml_text(xml_child(read_xml(response400), 2)) # making assumption that - body is second node
    statusMsg <- gsub(pattern=", server=.*", replacement="", x = statusReport)
    stop(statusMsg)
  } else if(status_code(returnedList) != 200){
    message("For: ", obs_url,"\n")
    stop_for_status(returnedList)
  } else {
    
    headerInfo <- headers(returnedList)

    if(headerInfo$`content-type` %in% c("text/tab-separated-values;charset=UTF-8")){
      returnedDoc <- content(returnedList, type="text",encoding = "UTF-8")
    } else if (headerInfo$`content-type` %in% 
               c("application/zip", 
                 "application/zip;charset=UTF-8",
                 "application/vnd.geo+json;charset=UTF-8")) {
      returnedDoc <- returnedList
    } else if (headerInfo$`content-type` %in% c("text/html",
                                                "text/html; charset=UTF-8") ){
      txt <- readBin(returnedList$content, character())
      message(txt)
      return(txt)
    } else {
      returnedDoc <- content(returnedList,encoding = "UTF-8")
      if(grepl("No sites/data found using the selection criteria specified", returnedDoc)){
        message(returnedDoc)
      }
    }

    attr(returnedDoc, "headerInfo") <- headerInfo

    return(returnedDoc)
  }
}

default_ua <- function() {
  versions <- c(
    libcurl = curl_version()$version,
    httr = as.character(packageVersion("httr")),
    dataRetrieval = as.character(packageVersion("dataRetrieval"))
  )
  paste0(names(versions), "/", versions, collapse = " ")
}

#' getting header information from a WQP query
#'
#'@param url the query url
#'@importFrom httr HEAD
#'@importFrom httr headers
getQuerySummary <- function(url){
  queryHEAD <- HEAD(url)
  retquery <- headers(queryHEAD)
  
  retquery[grep("-count",names(retquery))] <- as.numeric(retquery[grep("-count",names(retquery))])
  
  if("date" %in% names(retquery)){
    retquery$date <- as.Date(retquery$date, format = "%a, %d %b %Y %H:%M:%S")
  }
  
  return(retquery)
}

retryGetOrPost <- function(obs_url, ...) {
  resp <- NULL #|| grepl(pattern = "request=GetFeatureOfInterest", x= obs_url)
  if (nchar(obs_url) < 2048 ) {
    resp <- RETRY("GET", obs_url, ..., user_agent(default_ua()))
  } else if(!grepl(pattern = "ngwmn", x = obs_url)){
    split <- strsplit(obs_url, "?", fixed=TRUE)
    obs_url <- split[[1]][1]
    query <- split[[1]][2]
    resp <- RETRY("POST", obs_url, ..., body = query,
          content_type("application/x-www-form-urlencoded"), user_agent(default_ua()))
  } else { #NGWMN takes XML in POST body
    split <- strsplit(obs_url, "?", fixed=TRUE)
    obs_url <- split[[1]][1]
    query <- split[[1]][2]
    post_body <- build_NGWMN_post_body(query)
    resp <- RETRY("POST", obs_url, ..., body = as.character(post_body),
                  content_type("application/xml"), 
                  user_agent(default_ua()), httr::progress(), httr::verbose())
  }
  return(resp)
}


build_NGWMN_post_body <- function(query_string) {
  #add FOI nodes to this
  body <- xml2::read_xml('<?xml version="1.0" encoding="UTF-8"?>
    <sos:GetObservation xmlns:sos="http://www.opengis.net/sos/2.0">
                           <sos:offering>GW_LEVEL</sos:offering>
                           <sos:observedProperty>"urn:ogc:def:property:OGC:GroundWaterLevel"</sos:observedProperty>
                           </sos:GetObservation> ')
  #split out sites, construct child nodes
  sites <- unlist(strsplit(sub('.*\\=', '', query_string), split = ","))
  #not sure why xml_add_child adds a "<" to child nodes, leaving it off below
  site_nodes <- paste0("sos:featureOfInterest>", sites, "</sos:featureOfInterest>")
  for(node in site_nodes) {
    xml2::xml_add_child(body, node)
  }
  return(body)
}