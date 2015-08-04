
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
