
#####################
### Load packages ###
#####################

library(XML)
library(lubridate)
library(quantmod)
library(RMySQL)
# require(RCurl)
# require(XML)


if(file.exists("dbFunctions.R")) {
  source("dbFunctions.R")
  source("SourceTimeFunction.R") 
  source("SourceNewsFunctions.R")
  myHost <- "ec2-54-173-22-144.compute-1.amazonaws.com"
  myUser <- "remote"
  myPassword <- "nederland"
} else {
  source("/home/finance/GitFin/dbFunctions.R")
  source("/home/finance/GitFin/SourceTimeFunction.R")
  source("/home/finance/GitFin/SourceNewsFunctions.R")
  myHost <- "localhost"
  myUser <- "finance"
  myPassword <- "nederland"
}

###################################
### ALTER NEWS TABLE (RUN ONCE) ###
###################################

# Original table creation
# CREATE TABLE  basicnews  (
#   newsID  Integer(11) NOT NULL AUTO_INCREMENT,
#   timestamp  DATETIME NOT NULL,
#   source  varchar(255) NOT NULL, 
#   title  varchar(1000) NOT NULL, 
#   description  varchar(1000),
#   url  varchar(255),
#   tickerTags  varchar(255), 
#   PRIMARY KEY ( newsID ),
#   UNIQUE KEY uniq_TimeStamp_Source (timestamp, source)
# ) ENGINE=InnoDB DEFAULT CHARSET=latin1;

# Connect
# mydb = dbConnect(MySQL(), user=myUser, password=myPassword, host=myHost)
# on.exit(dbDisconnect(mydb))
# rs <- dbSendQuery(mydb, "USE finance;")
# 
# # Add table
# dbSendQuery(mydb, "ALTER TABLE basicnews ADD externalsource varchar(255);")
# 
# # Disconnect
# dbFinDisconnect()

# SELECT * FROM basicnews ORDER BY newsID DESC LIMIT 10;

########################
### ADD NOTIFICATION ###
########################

# Connect
mydb = dbConnect(MySQL(), user=myUser, password=myPassword, host=myHost)
on.exit(dbDisconnect(mydb))
rs <- dbSendQuery(mydb, "USE finance;")

# Add message
dbNotification("Starting Google Finance news collection process", 10)

# Disconnect
dbFinDisconnect()

#############################
# GET SYMBOLS FROM DATABASE #
#############################

# Set up connection
# Connect
mydb = dbConnect(MySQL(), user=myUser, password=myPassword, host=myHost)
on.exit(dbDisconnect(mydb))

# Query
rs <- dbSendQuery(mydb, "USE finance;")  
query <- "SELECT * FROM ticker WHERE isActive = 1;"
rs <- dbSendQuery(mydb, query)  
ticker <- fetch(rs)
row.names(ticker) <- ticker[,"symbol"]
print(ticker[1:6,c("symbol", "tickerID")])
selectSymbols <- as.vector(ticker[,"symbol"]) 

# Disconnect
dbDisconnect(mydb)

# Display symbols
print(selectSymbols)

###############################
### GET NEWS GOOGLE FINANCE ###
###############################

# https://www.google.com/finance/company_news?q=NASDAQ%3AAMZN

newsData <- c()
for(tNr in 1:nrow(ticker)) {
  
  # Compile url
  urlString <- paste("https://www.google.com/finance/company_news?q=NASDAQ%3A", ticker[tNr,"symbol"], sep="")
  print(urlString)
  
  # Pull data
  con = url(urlString); htmlCode = readLines(con); close(con)

  # Get section after: <div id="news-main". class="sfe-section">
  sRow <- grep("news-main", htmlCode, value=FALSE)
  htmlCode <- htmlCode[(sRow+1):length(htmlCode)]
  
  # Get section till: "</div>", "</div>", "<center>", "<div id=navbar>", "<table width=\"1%\" cellspacing=0 cellpadding=0>", 
  eRow <- grep("navbar", htmlCode, value=FALSE)
  htmlCode <- htmlCode[1:(eRow-1)]
  
  # Parse (less usefull for getting info out)
  doc <- htmlParse(htmlCode, asText = TRUE)
  
  # Loop through htmlCode
  for(lNr in 1:length(htmlCode)) {

    # Get elements
    if(grepl("href", htmlCode[lNr])) { link <- gsub("&.*$", "", gsub("^.*url=", "", htmlCode[lNr])) }  # Get news url
    if(grepl("href", htmlCode[lNr])) { ttle <- gsub("&nbsp;", " ",gsub("<.*?>","",htmlCode[lNr])) }      # Get title
    if(grepl("src.*date", htmlCode[lNr])) {  siteName <- gsub("<.*?>","",gsub(" - .*$", "", htmlCode[lNr])) }
    if(grepl("src.*date", htmlCode[lNr])) {  deltaTime <- gsub("<.*?>","",gsub("^.* - ", "", htmlCode[lNr])) }
    if(grepl("<div style=\"width:100%;\">", htmlCode[lNr])) { description <- gsub("&nbsp;", " ",gsub("<.*?>","",htmlCode[lNr])) }
  
    # Compile news item
    if(grepl("<div style=\"width:100%;\">", htmlCode[lNr])) {
      
      # Only compile news item if published minutes or hours ago
      if(grepl("hours", deltaTime)||grepl("minues", deltaTime)) {
        
        # Get time stamp
        cTime <- as.POSIXlt(Sys.time(), tz="America/New_York")
        if(grepl("hours", deltaTime)) {
          dHours <- as.numeric(gsub(" ", "",gsub("[:alpha:].*", "", deltaTime)))
        } else {
          dHours <- 0     
        }
        
        if(grepl("minutes", deltaTime)) {
          dMinutes <- as.numeric(gsub(" ", "",gsub("[:alpha:].*", "", deltaTime)))
        } else {
          dMinutes <- 0     
        }
        
        aTime <- cTime - 3600 * dHours - 60 * dMinutes
        timeStamp <- as.character(aTime)
        
        # Set tickers and external source
        tickers <- ticker[tNr,"symbol"]
        externalsource <- "Google"
        
        newsItem <- data.frame(timeStamp=timeStamp, 
                               siteName=siteName,
                               title=ttle, 
                               description=description,
                               link=link, 
                               tickers=tickers,
                               externalsource=externalsource,
                               stringsAsFactors = FALSE)
        print(newsItem)
        
        newsData <- rbind(newsData, newsItem)
      
      }
      
      # Clear other object
      link <- c()
      ttle <- c()
      siteName <- c()
      detlaDate <- c()
      description <- c()
      tickers <- c()
      externalsource <- c()
    
    }
    
  }
  
}

# Print size of news data
print(dim(newsData))

############################
### ADD DATA TO DATABASE ###
############################

recordsAdded <- 0  

# Loop and add
if(nrow(newsData)>0){
  
  # Connect
  mydb = dbConnect(MySQL(), user=myUser, password=myPassword, host=myHost)
  on.exit(dbDisconnect(mydb))
  rs <- dbSendQuery(mydb, "USE finance;")
  
  for(rowNr in 1:nrow(newsData)) {
    
    # Check exists
    curURL <- newsData[rowNr,"link"]
    curTickers <- newsData[rowNr, "tickers"]
    query <- paste("SELECT COUNT(*) FROM basicnews WHERE url='", curURL, "' AND tickerTags='", curTickers, "';", sep="")
    rs <- dbSendQuery(mydb, query)  
    if(fetch(rs)==0) { exist=FALSE } else { exist=TRUE }
    dbClearResult(dbListResults(mydb)[[1]])
    print(paste("News exists:",exist))
    
    # Add if doesn't exist
    if(exist==FALSE) {
      try(dbFinAdd("basicnews", c("timestamp", "source", "title", "description", "url", "tickerTags", "externalsource"), 
                   c(newsData[rowNr, c("timeStamp","siteName", "title", "description", "link",  "tickers", "externalsource")])))
      recordsAdded <- recordsAdded + 1
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
dbNotification(paste("Added", recordsAdded, "Google Finance news records", collapse=" "), 10)

# Disconnect
dbFinDisconnect()

