
# Load packages
  library(XML)
  library(lubridate)
  if(Sys.info()["nodename"]!="MA2") {
    source("/home/finance/GitFin/dbFunctions.R")
    source("/home/finance/GitFin/SourceTimeFunction.R")
  } else { 
    source("SourceTimeFunction.R") 
  } 
  

########################
### ADD NOTIFICATION ###
########################
  
  noticeText <- "Starting news collection process"
  
  mydb = dbConnect(MySQL(), user='finance', password='nederland', host='localhost')
  on.exit(dbDisconnect(mydb))
  rs <- dbSendQuery(mydb, "USE finance;")
  
  dbNotification(noticeText, 10)
  dbFinDisconnect()
  
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
    names(nData) <- gsub(".text", "", names(nData))
    nData <- unlist(nData)
    
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
  ProcessTop <- function(siteName, xmltop, xpath, timestampTag, titleTag, descTag, linkTag, getTicker) {
    nodes <- getNodeSet(xmltop, xpath)
    nodeData <- lapply(nodes, GetNodeDetails, timestampTag, titleTag, descTag, linkTag, getTicker)
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

# Show data collected
  head(aData); dim(aData)

####################
### PROCESS DATA ###
####################

  aData[,"title"] <- gsub("'", "", aData[,"title"])
  aData[,"description"] <- gsub("'", "", aData[,"description"])
  aData[,"timeStamp"] <- GetTime(aData[,"timeStamp"])
  aData <- aData[order(aData[,"timeStamp"], decreasing=TRUE), ]
  
  print(head(aData))
  print(dim(aData))

##############################
### GET INFO FROM DATABASE ###
##############################
  
# Connect
  mydb = dbConnect(MySQL(), user='finance', password='nederland', host='localhost')
  on.exit(dbDisconnect(mydb))
  rs <- dbSendQuery(mydb, "USE finance;")
  
# Get info
  rs <- dbSendQuery(mydb, "SELECT COUNT(*) FROM basicnews;")
  startNumberOfRecords <- fetch(rs)
  print(startNumberOfRecords)
  
  rs <- dbSendQuery(mydb, "SELECT MAX(timestamp) FROM basicnews;")
  dateTimeLastRecord <- fetch(rs)
  print(dateTimeLastRecord)
  
# Disconnect
  dbFinDisconnect()
  
############################
### SUBSET NEW DATA ONLY ###  
############################
  
  # dateTimeLastRecord <- "2015-08-01 14:31:47" # Debug line
  aData <- aData[as.POSIXct(aData[,"timeStamp"],tz="America/New_York") > ymd_hms(dateTimeLastRecord, tz="America/New_York") + seconds(30),]
  print(head(aData))
  
###################################
### ADD INFORMATION TO DATABASE ###
###################################

# Set up connection
# dbFinConnect()

# Connect
  mydb = dbConnect(MySQL(), user='finance', password='nederland', host='localhost')
  on.exit(dbDisconnect(mydb))
  rs <- dbSendQuery(mydb, "USE finance;")
  
# Enter data
  for(rowNr in 1:nrow(aData)) {
   dbFinAdd("basicnews", c("timestamp", "source", "title", "description", "url", "tickerTags"), 
            c(aData[rowNr, c("timeStamp","siteName", "title", "description", "link",  "tickers")])) 
  }
  
# Disconnect
  dbFinDisconnect()

########################
### ADD NOTIFICATION ###
########################

  noticeText <- "Added news"
  
  mydb = dbConnect(MySQL(), user='finance', password='nederland', host='localhost')
  on.exit(dbDisconnect(mydb))
  rs <- dbSendQuery(mydb, "USE finance;")
  dbNotification(noticeText, 10)

# Get info
  rs <- dbSendQuery(mydb, "SELECT COUNT(*) FROM basicnews;")
  endNumberOfRecords <- fetch(rs)
  recordsAdded <- endNumberOfRecords - startNumberOfRecords
  noticeText <- paste("Added ", recordsAdded, " news records", sep="")
  dbNotification(noticeText, 20)

# Disconnect 
  dbFinDisconnect()




