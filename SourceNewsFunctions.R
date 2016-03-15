
#################################################
### FUNCTIONS FOR PULLING AND PROCESSING DATA ###
#################################################

# Function to get data from url 
GetTop <- function(url) {
  con = url(url); sourceCode = readLines(con); close(con)
  xmlfile <- xmlTreeParse(sourceCode)
  xmltop = xmlRoot(xmlfile)  
  return(xmltop)
}

# Function to extract data from node
GetNodeDetails <- function(node, timeStampTag, titleTag, descTag, linkTag, getTicker) {
  
  # Process data
  nData <- xmlSApply(node, function(x) xmlSApply(x, xmlValue))
  nData <- unlist(nData)
  names(nData) <- gsub(".text", "", names(nData))
  
  # Get elements
  timeStamp <- nData[timeStampTag]
  timeStamp <- GetTime(timeStamp)
  title <- nData[titleTag]
  title <- gsub("'", "", title)
  if(!is.na(descTag)) { description <- nData[descTag] } else { description <- NA }
  description <- gsub("'", "", description)
  if(!is.na(linkTag)) { link <- nData[linkTag] } else { link <- NA }
  
  # Tickers
  if(getTicker) {
    tData <- as.data.frame(nData, stringsAsFactors=FALSE)
    tickers <- grep("^([A-Z]{1,5})$", tData[,1], value=TRUE)
    tickers <- paste(unique(tickers),collapse=",")
  } else {
    tickers <- NA
  }
  
  # Return data
  return(data.frame(timeStamp, title, description, link, tickers, row.names = NULL, stringsAsFactors=FALSE))
  
}

# Function to pull data and format it
ProcessTop <- function(siteName, xmltop, xpath="//rss/channel/item", timestampTag="pubDate", titleTag="title", descTag="description", linkTag="link", getTicker=FALSE) {
  nodes <- getNodeSet(xmltop, xpath)
  nodeData <- lapply(nodes, GetNodeDetails, timestampTag, titleTag, descTag, linkTag, getTicker)
  returnData <- as.data.frame(do.call(rbind, nodeData), stringsAsFactors = FAlSE)
  returnData <- cbind(siteName, returnData, stringsAsFactors=FALSE)
  return(returnData)
}

# Function to check data before adding it
CheckNewsData <- function(newsData, siteName) {
  
  # Assume fine 
  status <- TRUE
  errorText <- siteName
  
  # Check conditions
  if("factor" %in% apply(newsData,2,class)) { errorText <- paste(errorText,  "Contains factors", collapse="; "); status=FALSE }
  if(sum(names(newsData)!=c("siteName", "timeStamp", "title", "description", "link","tickers"))>0) { errorText <- paste(errorText,  "Incorrect column labels", collapse="; "); status=FALSE }
  if(sum(is.na(newsData[,1:3]))>0) { errorText <- paste(errorText,  "Contains invalid entries", collapse="; "); status=FALSE }
  
  # Return status and error if any
  
  if(status==FALSE) { 
    print(errorText)
    dbNotificationConnect(errorText, 1) 
  }  
  return(status)  
}


# https://github.com/tonybreyal/Blog-Reference-Functions/blob/master/R/htmlToText/htmlToText.R
# Pulled: 2016-03-05

# Author: Tony Breyal
# Date: 2011-11-18
# Modified: 2011-11-18
# Description: Extracts all text from a webpage (aims to extract only the text you would see in a web browser)
# Packages Used: RCurl, XML   
# Blog Reference: Not published

# Copyright (c) 2011, under the Creative Commons Attribution-NonCommercial 3.0 Unported (CC BY-NC 3.0) License
# For more information see: https://creativecommons.org/licenses/by-nc/3.0/
# All rights reserved.

htmlToText <- function(input, ...) {
  ###---PACKAGES ---###
  require(RCurl)
  require(XML)
  
  
  ###--- LOCAL FUNCTIONS ---###
  # Determine how to grab html for a single input element
  evaluate_input <- function(input) {    
    # if input is a .html file
    if(file.exists(input)) {
      char.vec <- readLines(input, warn = FALSE)
      return(paste(char.vec, collapse = ""))
    }
    
    # if input is html text
    if(grepl("</html>", input, fixed = TRUE)) return(input)
    
    # if input is a URL, probably should use a regex here instead?
    if(!grepl(" ", input)) {
      # downolad SSL certificate in case of https problem
      if(!file.exists("cacert.perm")) download.file(url="http://curl.haxx.se/ca/cacert.pem", destfile="cacert.perm")
      return(getURL(input, followlocation = TRUE, cainfo = "cacert.perm"))
    }
    
    # return NULL if none of the conditions above apply
    return(NULL)
  }
  
  # convert HTML to plain text
  convert_html_to_text <- function(html) {
    doc <- htmlParse(html, asText = TRUE)
    text <- xpathSApply(doc, "//text()[not(ancestor::script)][not(ancestor::style)][not(ancestor::noscript)][not(ancestor::form)]", xmlValue)
    return(text)
  }
  
  # format text vector into one character string
  collapse_text <- function(txt) {
    return(paste(txt, collapse = " "))
  }
  
  ###--- MAIN ---###
  # STEP 1: Evaluate input
  html.list <- lapply(input, evaluate_input)
  
  # STEP 2: Extract text from HTML
  text.list <- lapply(html.list, convert_html_to_text)
  
  # STEP 3: Return text
  text.vector <- sapply(text.list, collapse_text)
  return(text.vector)
}


