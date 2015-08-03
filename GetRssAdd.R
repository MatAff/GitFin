
# Load packages
  library(XML)
  library(lubridate)
  if(Sys.info()["nodename"]!="MA2") {
    source("/home/finance/GitFin/dbFunctions.R")
    source("/home/finance/GitFin/SourceTimeFunction.R")
  } else { 
    source("SourceTimeFunction.R") 
  } 
  
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
  title <- nData[titleTag]
  if(!is.na(descTag)) { description <- nData[descTag] } else { description <- NA }
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
  return(data.frame(timeStamp, title, description, link, tickers, row.names = NULL))
  
}

# Function to pull data and format it
ProcessTop <- function(siteName, xmltop, xpath, timeStampTag, titleTag, descTag, linkTag, getTicker) {
  nodes <- getNodeSet(xmltop, xpath)
  nodeData <- lapply(nodes, GetNodeDetails, timeStampTag, titleTag, descTag, linkTag, getTicker)
  returnData <- as.data.frame(do.call(rbind, nodeData), stringsAsFactors = FAlSE)
  returnData <- cbind(siteName, returnData)
  return(returnData)
}

#################
### PULL DATA ###
#################

# Create space to save data
aData <- c()

# SEEKING ALPHA
xmltop <- GetTop("http://seekingalpha.com/feed.xml")
newsData <- ProcessTop("seekingalpha.com", xmltop, "//channel/item", "pubDate", "title", NA, "link", TRUE)
aData <- rbind(aData, newsData)

# Motley Fool
xmltop <- GetTop("http://www.fool.com/feeds/index.aspx?id=foolwatch&format=rss2")
newsData <- ProcessTop("fool.com", xmltop, "//channel/item", "pubDate", "title", "description", "guid", TRUE)
aData <- rbind(aData, newsData)

# Wall Street Journal
xmltop <- GetTop("http://www.wsj.com/xml/rss/3_7031.xml")
newsData <- ProcessTop("wsj.com", xmltop, "//rss/channel/item", "pubDate", "title", "description", "link", FALSE)
aData <- rbind(aData, newsData)

# Financial Times
xmltop <- GetTop("http://www.ft.com/rss/markets")
newsData <- ProcessTop("ft.com", xmltop, "//rss/channel/item", "pubDate", "title", "description", "guid", FALSE)
aData <- rbind(aData, newsData)

# Forbes
xmltop <- GetTop("http://www.forbes.com/markets/index.xml")
newsData <- ProcessTop("forbes.com", xmltop, "//rss/channel/item", "pubDate", "title", "description", "link", FALSE)
aData <- rbind(aData, newsData) 
  

###############
### SANDBOX ###
###############
  

  
  xmltop <- GetTop("http://www.apple.com/pr/feeds/pr.rss")
  newsData <- ProcessTop("apple.com", xmltop, "//rss/channel/item", "pubDate", "title", "description", "link", FALSE)
  aData <- rbind(aData, newsData)  

  # Sub process
  xpath <- "//rss/channel/item"
  timeStampTag <- "pubDate"
  titleTag <- "title"
  descTag <- "description"
  linkTag <- "link"
  getTicker <- FALSE
  
  nodes <- getNodeSet(xmltop, xpath)
  nodeData <- lapply(nodes, GetNodeDetails, timeStampTag, titleTag, descTag, linkTag, getTicker)
  returnData <- as.data.frame(do.call(rbind, nodeData), stringsAsFactors = FAlSE)
  lapply(nodeData, names)
  
  node <- nodes[[2]]
  nData <- xmlSApply(node, function(x) xmlSApply(x, xmlValue))
  nData <- unlist(nData)
  names(nData) <- gsub(".text", "", names(nData))
  nData
    
 
  
  # Get elements
  timeStamp <- nData[timeStampTag]
  title <- nData[titleTag]
  if(!is.na(descTag)) { description <- nData[descTag] } else { description <- NA }
  if(!is.na(linkTag)) { link <- nData[linkTag] } else { link <- NA }
  
  # Tickers
  if(getTicker) {
    tData <- as.data.frame(nData, stringsAsFactors=FALSE)
    tickers <- grep("^([A-Z]{1,5})$", tData[,1], value=TRUE)
    tickers <- paste(unique(tickers),collapse=",")
  } else {
    tickers <- NA
  }
  
  data.frame(timeStamp, title, description, link, tickers, row.names = NULL)
  
  
  
  
  
  http://podcast.cnbc.com/mmpodcast/fastmoney.xml
  
  
head(newsData)
  
head(aData)
  
  


####################
### PROCESS DATA ###
####################

aData[,"title"] <- gsub("'", "", aData[,"title"])
aData[,"description"] <- gsub("'", "", aData[,"description"])
aData[,"timeStamp"] <- GetTime(aData[,"timeStamp"])
aData <- aData[order(aData[,"timeStamp"], decreasing=TRUE), ]

print(head(aData))
print(dim(aData))

