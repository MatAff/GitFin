
# Load packages
  library(XML)

#################################################
### FUNCTIONS FOR PULLING AND PROCESSING DATA ###
#################################################
  
# Function get data from url 
  GetTop <- function(url) {
    con = url(url); sourceCode = readLines(con); close(con)
    xmlfile <- xmlTreeParse(sourceCode)
    xmltop = xmlRoot(xmlfile)  
    return(xmltop)
  }

# Function to extract data from node
  GetNodeDetails <- function(node, timestampTag, titleTag, descTag, linkTag, getTicker) {
  
    # Process data
     nData <- xmlSApply(node, function(x) xmlSApply(x, xmlValue))
     names(nData) <- gsub(".text", "", names(nData))
  
    # Get elements
     timeStamp <- nData[timestampTag]
     title <- nData[titleTag]
     if(!is.na(descTag)) { description <- nData[descTag] } else { description <- NA }
     if(!is.na(linkTag)) { link <- nData[linkTag] } else { link <- NA }
  
  # Tickers
    if(getTicker) {
      tData <- as.data.frame(nData, stringsAsFactors=FALSE)
      tickers <- grep("^([A-Z]{1,5})$", tData[,1], value=TRUE)
      tickers <- unique(tickers)
      tickers <- paste(tickers,collapse=",")
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
    returnData <- as.data.frame(do.call(rbind, nodeData))
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

####################################
### SUBSET DATA RECENT NEWS ONLY ###
####################################

# Create non-factor data
  aData[,"siteName"] <- levels(aData[,"siteName"])[aData[,"siteName"]]
  aData[,"timeStamp"] <- levels(aData[,"timeStamp"])[aData[,"timeStamp"]]
  aData[,"title"] <- levels(aData[,"title"])[aData[,"title"]]
  aData[,"description"] <- levels(aData[,"description"])[aData[,"description"]]
  aData[,"link"] <- levels(aData[,"link"])[aData[,"link"]]
  aData[,"tickers"] <- levels(aData[,"tickers"])[aData[,"tickers"]]
  
Sys.time()  

# time standard problem - Convert everything to Eastern
  
myDate <- aData[1,"timeStamp"]
myDate
myDate <- "28 Jul 2015 21:18:59"  
as.Date(myDate)  
  
  -0400 means difference from UTC (So probably east coast time corrected for daylight saving)
  

myDate <- aData[70,"timeStamp"]


as.POSIXct(myDate)
  
  
# How to check an entry doesn't already exist
# News is continuous
# Don't need to add data older than the last running
# Boderline cases we might need to check 
# Pull data every 15 minutes

#




# Debug info
#xpath <- "//channel/item"
#siteName <- "seekingalpha.com"
#timestampTag <- "pubDate"
#titleTag <- "title"
#descTag <- "title"
#linkTag <- "link"




# Display data

# Write to database
library(RMySQL)

# Set up connection
mydb = dbConnect(MySQL(), user='finance', password='nederland', host='localhost')
on.exit(dbDisconnect(con))

# Select database
rs <- dbSendQuery(mydb, "USE finance;")

# Enter data
for(rowNr in 1:nrow(rData)) {
  query <- paste("INSERT INTO news (n_timestamp, site, title, description, link, tickers) 
          VALUES ('", rData[rowNr, "timeStamp"], "', '", rData[rowNr, "site"], 
        "', '", rData[rowNr, "title"], "', '", rData[rowNr, "description"]
        , "', '", rData[rowNr, "link"], "', '", rData[rowNr, "tickers"], "');", sep="")
  print(query)
  rs <- dbSendQuery(mydb, query)
}

# Disconnect
dbDisconnect(mydb)
