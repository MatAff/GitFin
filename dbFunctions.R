
# Load packages
library(RMySQL)

# Connect to database - Set up connection and select database
dbFinConnect <- function() {
  mydb = dbConnect(MySQL(), user='finance', password='nederland', host='localhost')
  on.exit(dbDisconnect(mydb))
  rs <- dbSendQuery(mydb, "USE finance;")
}

# Disconnect from database
dbFinDisconnect <- function() { dbDisconnect(mydb) }

# Add info to database
dbFinAdd <- function(table, fields, values) {
  fields <- paste(fields, collapse=", ")
  values <- paste(values, collapse="', '")
  query <- paste("INSERT INTO ", table, " (", fields, ") VALUES ('", values, "');", sep="")
  print(query)
  rs <- dbSendQuery(mydb, query)  
}

# Test function
#table <- "Test"
#fields <- c("n_timestamp", "site", "title", "description", "link", "tickers")
#values <- c("aaa", "aaa", "titles", "blablabla", "wwww.com", "abcde")
#dbFinAdd(table, fields, values)

