
# Load packages
library(RMySQL)

# Set up connection
mydb = dbConnect(MySQL(), user='finance', password='nederland', host='localhost')

# Check if table exists
rs <- dbSendQuery(mydb, "SELECT * FROM information_schema.tables
  WHERE table_schema = 'finance' 
  AND table_name = 'prices'
  LIMIT 1;")
mydbData <- fetch(rs, n=10)
tableExists <- nrow(mydbData)!=0
#dbClearResult(mydb)

# Create table if it doesn't exist
if(tableExists==FALSE) {
  rs <- dbSendQuery(mydb, "USE finance;")
  rs <- dbSendQuery(mydb, "CREATE TABLE prices
    ( p_timestamp TIMESTAMP(6),
      ticker varchar(255),
      price DECIMAL(8,3)  );")
} else {
  print("Table exists")
}

# Disconnect
dbDisconnect(mydb)

