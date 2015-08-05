
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
  if(T) {
    url <- "http://seekingalpha.com/feed.xml"; siteName <- "seekingalpha.com"
    print(paste("Now getting data from:", siteName))
    xmltop <- GetTop(url)
    newsData <- ProcessTop(siteName, xmltop, descTag=NA, getTicker=FALSE)
    if(CheckNewsData(newsData, siteName)==TRUE) { aData <- rbind(aData, newsData) }
  }

# Motley Fool
  if(T) {
    url <- "http://www.fool.com/feeds/index.aspx?id=foolwatch&format=rss2"; siteName <- "fool.com"
    print(paste("Now getting data from:", siteName))
    xmltop <- GetTop(url)
    newsData <- ProcessTop(siteName, xmltop, linkTag="guid", getTicker=TRUE)
    if(CheckNewsData(newsData, siteName)==TRUE) { aData <- rbind(aData, newsData) }
  }

# Wall Street Journal 
  if(T) {
    url <- "http://www.wsj.com/xml/rss/3_7031.xml"; siteName <- "wsj.com"
    print(paste("Now getting data from:", siteName))
    xmltop <- GetTop(url)
    newsData <- ProcessTop(siteName, xmltop, xpath="//channel/item", linkTag="guid")
    if(CheckNewsData(newsData, siteName)==TRUE) { aData <- rbind(aData, newsData) }
  }

# Financial Times
  if(T) {
    url <- "http://www.ft.com/rss/markets"; siteName <- "ft.com"
    print(paste("Now getting data from:", siteName))
    xmltop <- GetTop(url)
    newsData <- ProcessTop(siteName, xmltop, linkTag="guid")
    if(CheckNewsData(newsData, siteName)==TRUE) { aData <- rbind(aData, newsData) }
  }
  
# Forbes  
  if(T) {
    try(url <- "http://www.forbes.com/markets/index.xml"
    siteName <- "forbes.com"
    print(paste("Now getting data from:", siteName))
    xmltop <- GetTop(url)
    newsData <- ProcessTop(siteName, xmltop)
    if(CheckNewsData(newsData, siteName)==TRUE) { aData <- rbind(aData, newsData) })
  }
  
# Market Watch  
  if(T) {
    url <- "http://www.marketwatch.com/rss/newsfinder/AllMarketWatchNews/?p=type&pv=Stocks%20to%20Watch&t=Stocks%20to%20Watch&dist=sr_rss"; siteName <- "marketwatch.com"
    print(paste("Now getting data from:", siteName))
    xmltop <- GetTop(url)
    newsData <- ProcessTop(siteName, xmltop)
    if(CheckNewsData(newsData, siteName)==TRUE) { aData <- rbind(aData, newsData) }
  }
  
# Apple.com  
 # url <- "http://www.apple.com/pr/feeds/pr.rss"; siteName <- "apple.com"
  #print(paste("Now getting data from:", siteName))
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
  timeCutOff <- timeNow - hours(2); print(paste("Cut off time:", timeCutOff))
  aData <- aData[aData[,"timeStamp"]>timeCutOff, ]
  head(aData); dim(aData)

###################################
### ADD INFORMATION TO DATABASE ###
###################################

recordsAdded <- 0  
  
# Loop and add
  if(nrow(aData)>0){
    
    # Connect
    mydb = dbConnect(MySQL(), user=myUser, password=myPassword, host=myHost)
    on.exit(dbDisconnect(mydb))
    rs <- dbSendQuery(mydb, "USE finance;")
    
    for(rowNr in 1:nrow(aData)) {
  
    # Check exists
      curTitle <- aData[rowNr,"title"]
      curTimeStamp <- gsub(" [A-Z]{3}", "", aData[rowNr,"timeStamp"])
      query <- paste("SELECT COUNT(*) FROM basicnews WHERE title='", curTitle, "' AND timestamp='", curTimeStamp, "';", sep="")
      rs <- dbSendQuery(mydb, query)  
      if(fetch(rs)==0) { exist=FALSE } else { exist=TRUE }
      dbClearResult(dbListResults(mydb)[[1]])
      print(paste("News exists:",exist))
                   
    # Add if doesn't exist
      if(exist==FALSE) {
        try(dbFinAdd("basicnews", c("timestamp", "source", "title", "description", "url", "tickerTags"), 
               c(aData[rowNr, c("timeStamp","siteName", "title", "description", "link",  "tickers")])) 
        recordsAdded <- recordsAdded + 1)
      }
  }
  } else {
    print("no news data to add")
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
  dbNotification(paste("Added", recordsAdded, "news records", collapse=" "), 10)
  
# Disconnect
  dbFinDisconnect()
  
