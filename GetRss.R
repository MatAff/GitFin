
# Load packages
  library(XML)
  library(lubridate)
  if(Sys.info()["nodename"]!="MA2") {
    source("/home/finance/GitFin/dbFunctions.R")
    source("/home/finance/GitFin/SourceTimeFunction.R")
    source("/home/finance/GitFin/SourceNewsFunctions.R")
    myHost <- "localhost"
    myUser <- "finance"
    myPassword <- "nederland"
  } else { 
    source("SourceTimeFunction.R") 
    source("dbFunctions.R") 
    source("SourceNewsFunctions.R")
    myHost <- "ec2-54-173-22-144.compute-1.amazonaws.com"
    myUser <- "remote"
    myPassword <- "nederland"
    
  } 
  
########################
### ADD NOTIFICATION ###
########################
  
# Connect
  mydb = dbConnect(MySQL(), user=myUser, password=myPassword, host=myHost)
  on.exit(dbDisconnect(mydb))
  rs <- dbSendQuery(mydb, "USE finance;")

# Add message
  dbNotification("Starting news collection process", 10)
  
# Disconnect
  dbFinDisconnect()
  
#################
### PULL DATA ###
#################

# Create space to save data
  aData <- c()

# SEEKING ALPHA
  url <- "http://seekingalpha.com/feed.xml"; siteName <- "seekingalpha.com"
  xmltop <- GetTop(url)
  newsData <- ProcessTop(siteName, xmltop, descTag=NA, getTicker=FALSE)
  if(CheckNewsData(newsData, siteName)==TRUE) { aData <- rbind(aData, newsData) }

# Motley Fool
  url <- "http://www.fool.com/feeds/index.aspx?id=foolwatch&format=rss2"; siteName <- "fool.com"
  xmltop <- GetTop(url)
  newsData <- ProcessTop(siteName, xmltop, linkTag="guid", getTicker=TRUE)
  if(CheckNewsData(newsData, siteName)==TRUE) { aData <- rbind(aData, newsData) }

# Wall Street Journal 
  url <- "http://www.wsj.com/xml/rss/3_7031.xml"; siteName <- "wsj.com"
  xmltop <- GetTop(url)
  newsData <- ProcessTop(siteName, xmltop, xpath="//channel/item", linkTag="guid")
  if(CheckNewsData(newsData, siteName)==TRUE) { aData <- rbind(aData, newsData) }

# Financial Times
  url <- "http://www.ft.com/rss/markets"; siteName <- "ft.com"
  xmltop <- GetTop(url)
  newsData <- ProcessTop(siteName, xmltop, linkTag="guid")
  if(CheckNewsData(newsData, siteName)==TRUE) { aData <- rbind(aData, newsData) }
  
# Forbes  
  url <- "http://www.forbes.com/markets/index.xml"; siteName <- "forbes.com"
  xmltop <- GetTop(url)
  newsData <- ProcessTop(siteName, xmltop)
  if(CheckNewsData(newsData, siteName)==TRUE) { aData <- rbind(aData, newsData) }
  
# Market Watch  
  url <- "http://www.marketwatch.com/rss/newsfinder/AllMarketWatchNews/?p=type&pv=Stocks%20to%20Watch&t=Stocks%20to%20Watch&dist=sr_rss"; siteName <- "marketwatch.com"
  xmltop <- GetTop(url)
  newsData <- ProcessTop(siteName, xmltop)
  if(CheckNewsData(newsData, siteName)==TRUE) { aData <- rbind(aData, newsData) }
  
# Apple.com  
 # url <- "http://www.apple.com/pr/feeds/pr.rss"; siteName <- "apple.com"
  #xmltop <- GetTop(url)
  #newsData <- ProcessTop(siteName, xmltop)
  #if(CheckNewsData(newsData, siteName)==TRUE) { aData <- rbind(aData, newsData) }
  
# Show data collected
  head(aData); dim(aData)
  
####################
### PROCESS DATA ###
####################

  # Sort by date and time
  aData <- aData[order(aData[,"timeStamp"], decreasing=TRUE), ]
  
  # Subset based on time
  timeNow <- as.POSIXlt(Sys.time(), "America/New_York") #format(Sys.time(), "%F %H:%M:%S")
  timeCutOff <- timeNow - hours(2)
  aData <- aData[aData[,"timeStamp"]>timeCutOff, ]
  head(aData); dim(aData)

###################################
### ADD INFORMATION TO DATABASE ###
###################################

recordsAdded <- 0  
  
# Connect
  mydb = dbConnect(MySQL(), user=myUser, password=myPassword, host=myHost)
  on.exit(dbDisconnect(mydb))
  rs <- dbSendQuery(mydb, "USE finance;")
  
# Loop and add
  for(rowNr in 1:nrow(aData)) {
  
    # Check exists
      curTitle <- aData[rowNr,"title"]
      query <- paste("SELECT COUNT(*) FROM basicnews WHERE title='", curTitle, "';", sep="")
      rs <- dbSendQuery(mydb, query)  
      if(fetch(rs)==0) { exist=FALSE } else { exist=TRUE }
      print(exist)
                   
    # Add if doesn't exist
      if(exist==FALSE) {
        dbFinAdd("basicnews", c("timestamp", "source", "title", "description", "url", "tickerTags"), 
               c(aData[rowNr, c("timeStamp","siteName", "title", "description", "link",  "tickers")])) 
        recordsAdded <- recordsAdded + 1
      }
  }
  
# Disconnect
  dbFinDisconnect()

########################
### ADD NOTIFICATION ###
########################

# Connect
  mydb = dbConnect(MySQL(), user=myUser, password=myPassword, host=myHost)
  on.exit(dbDisconnect(mydb))
  rs <- dbSendQuery(mydb, "USE finance;")
  
# Add message
  dbNotificationConnect(paste("Added", recordsAdded, "news records", collapse=" "), 10)
  
# Disconnect
  dbFinDisconnect()
  
  
  
  
### OLD CODE BELOW ### OLD CODE BELOW ### OLD CODE BELOW ### OLD CODE BELOW ### OLD CODE BELOW ### OLD CODE BELOW ###
  
  ##############################
  ### GET INFO FROM DATABASE ###
  ##############################
  
  # Connect
  #mydb = dbConnect(MySQL(), user='finance', password='nederland', host='localhost')
  #on.exit(dbDisconnect(mydb))
  #rs <- dbSendQuery(mydb, "USE finance;")
  
  # Get info
  #rs <- dbSendQuery(mydb, "SELECT COUNT(*) FROM basicnews;")
  #startNumberOfRecords <- fetch(rs)
  #print(startNumberOfRecords)
  
  # Disconnect
  #dbFinDisconnect()
  
  
  # rs <- dbSendQuery(mydb, "SELECT MAX(timestamp) FROM basicnews;")
  #  dateTimeLastRecord <- fetch(rs)
  #  print(dateTimeLastRecord)
  

  ############################
  ### SUBSET NEW DATA ONLY ###  
  ############################
  
  # dateTimeLastRecord <- "2015-08-01 14:31:47" # Debug line
  #aData <- aData[as.POSIXct(aData[,"timeStamp"],tz="America/New_York") > ymd_hms(dateTimeLastRecord, tz="America/New_York") + seconds(30),]
  #print(head(aData))
  
  #for(rowNr in 1:nrow(aData)) {
  #  dbFinAdd("basicnews", c("timestamp", "source", "title", "description", "url", "tickerTags"), 
  #           c(aData[rowNr, c("timeStamp","siteName", "title", "description", "link",  "tickers")])) 
  #}
  
  
  #noticeText <- "Added news"
  
  #mydb = dbConnect(MySQL(), user='finance', password='nederland', host='localhost')
  #on.exit(dbDisconnect(mydb))
  #rs <- dbSendQuery(mydb, "USE finance;")
  #dbNotification(noticeText, 10)
  
  # Get info
  #rs <- dbSendQuery(mydb, "SELECT COUNT(*) FROM basicnews;")
  #endNumberOfRecords <- fetch(rs)
  #recordsAdded <- endNumberOfRecords - startNumberOfRecords
  #noticeText <- paste("Added ", recordsAdded, " news records", sep="")
  #dbNotification(noticeText, 20)
  
  # Disconnect 
  #dbFinDisconnect()
  
  