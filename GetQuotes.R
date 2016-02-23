
print("HI")

# Load packages
  library(quantmod)
  library(RMySQL)
  source("/home/finance/GitFin/dbFunctions.R")

  
  
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
  for(sNr in 1:nrow(qData)) {
    tickerID <- ticker[qData[sNr,"symbol"], "tickerID"]
    print(tickerID)
    query <- paste("INSERT INTO quote (timestamp, tickerID, price)
      VALUES ('", qData[sNr, "Trade Time"], "', '", tickerID, "', '", qData[sNr, "Last"], "');", sep="")
    print(query)
    rs <- dbSendQuery(mydb, query)
  }
  
# Disconnect
  dbDisconnect(mydb)
  
########################
### ADD NOTIFICATION ###
########################
  
  noticeText <- paste("Added ", nrow(aData), " quotes to quote table", sep = "")
    
  mydb = dbConnect(MySQL(), user='finance', password='nederland', host='localhost')
  on.exit(dbDisconnect(mydb))
  rs <- dbSendQuery(mydb, "USE finance;")
  
  dbNotification(noticeText, 10)
  dbFinDisconnect()
      

  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  