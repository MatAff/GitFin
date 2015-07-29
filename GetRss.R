
# Load packages
library(XML)

# Function to get top
GetTop <- function(url) {
  con = url(url); htmlCode = readLines(con); close(con)
  xmlfile <- xmlTreeParse(htmlCode)
  xmltop = xmlRoot(xmlfile)  
  return(xmltop)
}

# Function to extract data from node
GetNodeDetails <- function(node, timestampTag, titleTag, descTag, linkTag) {
  
  # Process data
  nData <- xmlSApply(node, function(x) xmlSApply(x, xmlValue))
  names(nData) <- gsub(".text", "", names(nData))
  
  # Get elements
  timeStamp <- nData[timestampTag]
  title <- nData[titleTag]
  description <- nData[descTag]
  link <- nData[linkTag]
  
  # Tickers
  tData <- as.data.frame(nData, stringsAsFactors=FALSE)
  tickers <- grep("^([A-Z]{1,5})$", tData[,1], value=TRUE)
  tickers <- unique(tickers)
  tickers <- paste(tickers,collapse=",")
  
  # Create data frame to return
  returnData <- data.frame(timeStamp, title, description, link, tickers, row.names = NULL)
  
  # Return data frame
  return(returnData)
  
}

ProcessTop <- function(siteName, xmltop, xpath, timestampTag, titleTag, descTag, linkTag) {

  nodes <- getNodeSet(xmltop, xpath)
  nodeData <- lapply(nodes, GetNodeDetails, timestampTag, titleTag, descTag, linkTag)
  returnData <- as.data.frame(do.call(rbind, nodeData))
  returnData <- cbind(siteName, returnData)
  return(returnData)
  
}

allNewsData <- c()

# SEEKING ALPHA
xmltop <- GetTop("http://seekingalpha.com/feed.xml")
newsData <- ProcessTop("seekingalpha.com", xmltop, "//channel/item", "pubDate", "title", "title", "link")
allNewsData <- rbind(allNewsData, newsData)

# Motley Fool
xmltop <- GetTop("http://www.fool.com/feeds/index.aspx?id=foolwatch&format=rss2")
newsData <- ProcessTop("fool.com", xmltop, "//channel/item", "pubDate", "title", "description", "guid")
allNewsData <- rbind(allNewsData, newsData)

# Wall Street Journal
xmltop <- GetTop("http://www.wsj.com/xml/rss/3_7031.xml")
newsData <- ProcessTop("wsj.com", xmltop, "//rss/channel/item", "pubDate", "title", "description", "link")
allNewsData <- rbind(allNewsData, newsData)

 

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
