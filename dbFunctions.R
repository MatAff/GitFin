
# Load packages
library(RMySQL)

if(Sys.info()["nodename"]!="MA2") {
  myHost <- "localhost"
  myUser <- "finance"
  myPassword <- "nederland"
} else { 
  myHost <- "ec2-54-173-22-144.compute-1.amazonaws.com"
  myUser <- "remote"
  myPassword <- "nederland"
}

# Connect to database - Set up connection and select database
dbFinConnect <- function() {
  mydb = dbConnect(MySQL(), user=myUser, password=myPassword, host=myHost)
  on.exit(dbDisconnect(mydb))
  rs <- dbSendQuery(mydb, "USE finance;")
  assign("mydb", mydb, envir = .GlobalEnv)
}

# Disconnect from database
dbFinDisconnect <- function() { dbDisconnect(mydb) }

# Add info to database
dbFinAdd <- function(table, fields, values) {
  fields <- paste(fields, collapse=", ")
  values <- paste(values, collapse="', '")
  query <- paste("INSERT INTO ", table, " (", fields, ") VALUES ('", values, "');", sep="")
  print(query)
  try(rs <- dbSendQuery(mydb, query))
  dbClearResult(dbListResults(mydb)[[1]])
}

# Test function
#table <- "Test"
#fields <- c("n_timestamp", "site", "title", "description", "link", "tickers")
#values <- c("aaa", "aaa", "titles", "blablabla", "wwww.com", "abcde")
#dbFinAdd(table, fields, values)

# Add notification
dbNotification <- function(notice, importance) {
  timeStamp <- as.character(as.POSIXlt(Sys.time(), "America/New_York"))
  dbFinAdd("notification", c("timestamp", "notice", "importance"), c(timeStamp, notice, importance))
  #format(Sys.time(), "%F %H:%M:%S")
}

# Add notification
dbNotificationConnect <- function(notice, importance) {
  
  # Connect 
  mydb = dbConnect(MySQL(), user=myUser, password=myPassword, host=myHost)
  on.exit(dbDisconnect(mydb))
  rs <- dbSendQuery(mydb, "USE finance;")
  
  # Add notice
  timeStamp <- dateTimeText <- as.character(as.POSIXlt(Sys.time(), "America/New_York"))
  table <- "notification"
  fields <- c("timestamp", "notice", "importance")
  values <- c(timeStamp, notice, importance)
  
  fields <- paste(fields, collapse=", ")
  values <- paste(values, collapse="', '")
  query <- paste("INSERT INTO ", table, " (", fields, ") VALUES ('", values, "');", sep="")
  print(query)
  rs <- dbSendQuery(mydb, query)  
  
  # Disconnect
  dbDisconnect(mydb)
  
}

