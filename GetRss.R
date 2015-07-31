# Load packages
library(XML)
source("dbFunctions.R")

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

# Function to process time information
GetTime <- function(dateTimeText) {
  
  # Get year
  year <- unlist(regmatches(dateTimeText, gregexpr("201[0-9]", dateTimeText)))
  dateTimeText <- sapply(1:length(dateTimeText), function(x) gsub(year[x], "", dateTimeText[x]))
  print(year); print(dateTimeText)
  
  # Get weekday
  daysVec <- c("mon|tue|wed|thu|fri|sat|sun|Mon|Tue|Wed|Thu|Fri|Sat|Sun")
  weekDay <- unlist(regmatches(dateTimeText, gregexpr(daysVec, dateTimeText)))
  dateTimeText <- sapply(1:length(dateTimeText), function(x) gsub(weekDay[x], "", dateTimeText[x]))
  print(weekDay); print(dateTimeText)
  
  # Get time zone
  timeZone <- unlist(regmatches(dateTimeText, gregexpr("[A-Z]{3}|-[0-9]{2}00", dateTimeText)))
  dateTimeText <- sapply(1:length(dateTimeText), function(x) gsub(timeZone[x], "", dateTimeText[x]))
  print(timeZone); print(dateTimeText)
  
  # Get time section
  timeText <- unlist(regmatches(dateTimeText, gregexpr("[0-2][0-9]:[0-5][0-9]:[0-5][0-9]", dateTimeText)))
  dateTimeText <- sapply(1:length(dateTimeText), function(x) gsub(timeText[x], "", dateTimeText[x]))
  print(timeText); print(dateTimeText)
  
  # Get month tag
  month <- regmatches(dateTimeText, gregexpr("[a-zA-Z]{3}", dateTimeText))
  monthTags <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  monthNrs <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")
  for(m in 1:12) { month <- sapply(1:length(month), function(x) gsub(monthTags[m], monthNrs[m], month[x])) }
  
  dateTimeText <- sapply(1:length(dateTimeText), function(x) gsub(month[x], "", dateTimeText[x]))
  print(month); print(dateTimeText)
  
  # Date
  date <- regmatches(dateTimeText, gregexpr(" [0-9]{2} ", dateTimeText))
  date <- gsub(" ", "", date)
  dateTimeText <- sapply(1:length(dateTimeText), function(x) gsub(date[x], "", dateTimeText[x]))
  print(date); print(dateTimeText)
  
  # Compile date string
  dateTimeText <- paste(year,"-",month,"-",date, " ", timeText, sep="")
  # dateTimeText <- as.Date(dateTimeText)
  
  # Return dateTimeText
  return(dateTimeText)
  
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

####################
### PROCESS DATA ###
####################

# Create non-factor data
aData[,"siteName"] <- levels(aData[,"siteName"])[aData[,"siteName"]]
aData[,"timeStamp"] <- levels(aData[,"timeStamp"])[aData[,"timeStamp"]]
aData[,"title"] <- levels(aData[,"title"])[aData[,"title"]]
# aData[,"description"] <- levels(aData[,"description"])[aData[,"description"]]
aData[,"link"] <- levels(aData[,"link"])[aData[,"link"]]
aData[,"tickers"] <- levels(aData[,"tickers"])[aData[,"tickers"]]
aData[,"timeStamp"] <- GetTime(aData[,"timeStamp"])

###################################
### ADD INFORMATION TO DATABASE ###
###################################

# Set up connection
# dbFinConnect()

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


