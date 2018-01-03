
# OVERALL PROCESS
# Load packages
# Add starting notification
# Get active tickers from database
# Subset valid data (Remove quotes that don't have today's date)
# Check if data exists in database and add if not
# Add notification to notification table about how many quotes were added

#####################
### Load packages ###
#####################

library(quantmod)
library(RMySQL)

if(file.exists("dbFunctions.R")) {
  source("dbFunctions.R")
} else {
  source("/home/finance/GitFin/dbFunctions.R")  
}

########################
### ADD NOTIFICATION ###
########################
  
  noticeText <- "Starting quote collection process"
  mydb = dbConnect(MySQL(), user='finance', password='nederland', host='localhost')
  on.exit(dbDisconnect(mydb))
  rs <- dbSendQuery(mydb, "USE finance;")
  dbNotification(noticeText, 10)
  dbFinDisconnect()
  
#############################
# GET SYMBOLS FROM DATABASE #
#############################
  
# Set up connection
  mydb = dbConnect(MySQL(), user='finance', password='nederland', host='localhost')
  on.exit(dbDisconnect(con))
  
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

##############
# GET QUOTES #
##############

# Debug sample symbols
  # selectSymbols <- c("AAPL", "ADBE", "ADI", "ADP", "ADSK", "AKAM", "ALTR", "ALXN", "AMAT", "AMGN", "AMZN")  
  
# Get quote
  qData <- getQuote(selectSymbols)
  qData[,"symbol"] <- row.names(qData)
  
# Print data to screen - for dev purposes
  print(qData[1:6,c("symbol", "Trade Time", "Last")])

########################
# ADD DATA TO DATABASE #
########################
  
# Set up connection
  mydb = dbConnect(MySQL(), user='finance', password='nederland', host='localhost')
  on.exit(dbDisconnect(con))

# Select database
  rs <- dbSendQuery(mydb, "USE finance;")

# Enter data
  addedRecords <- 0
  for(sNr in 1:nrow(qData)) {
    tickerID <- ticker[qData[sNr,"symbol"], "tickerID"]; print(tickerID)
    
    # Check if data exists
    # SELECT COUNT(*) FROM quote WHERE timestamp='2016-02-24 04:00:00' AND tickerID='200';
    query <- paste("SELECT COUNT(*) FROM quote WHERE timestamp='", qData[sNr, "Trade Time"], "' AND tickerID='", tickerID, "';", sep="")
    rs <- dbSendQuery(mydb, query)  
    countData <- fetch(rs)
    if(countData==1) { 
      print("Record already exists, skipping") 
    } else {
      
      # Add data to quote table
      query <- paste("INSERT INTO quote (timestamp, tickerID, price)
      VALUES ('", qData[sNr, "Trade Time"], "', '", tickerID, "', '", qData[sNr, "Last"], "');", sep="")
      print(query)
      rs <- dbSendQuery(mydb, query)
      
      addedRecords <- addedRecords + 1
        
    }

  }
  
# Disconnect
  dbDisconnect(mydb)
  
########################
### ADD NOTIFICATION ###
########################
  
  noticeText <- paste("Added ", addedRecords, " quotes to quote table", sep = "")
    
  mydb = dbConnect(MySQL(), user='finance', password='nederland', host='localhost')
  on.exit(dbDisconnect(mydb))
  rs <- dbSendQuery(mydb, "USE finance;")
  
  dbNotification(noticeText, 10)
  dbFinDisconnect()
      

  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
