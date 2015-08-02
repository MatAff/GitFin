
# Load packages
source("dbFunctions.R")

# Connect to datebase
mydb = dbConnect(MySQL(), user='finance', password='nederland', host='localhost')
on.exit(dbDisconnect(mydb))
rs <- dbSendQuery(mydb, "USE finance;")

# Get content

# Quotes
nrActiveTickers <- fetch(dbSendQuery(mydb, "SELECT COUNT(*) FROM ticker WHERE isActive = 1;"))
nrQuotesTotal <- fetch(dbSendQuery(mydb, "SELECT COUNT(*) FROM quote;"))
nrQuotesToday <- fetch(dbSendQuery(mydb, "SELECT COUNT(*) FROM quote WHERE timestamp >= CURDATE();"))

# News
nrNewsRecords <- fetch(dbSendQuery(mydb, "SELECT COUNT(*) FROM basicnews;"))
nrNewsRecordsToday <- fetch(dbSendQuery(mydb, "SELECT COUNT(*) FROM basicnews WHERE timestamp >= CURDATE();"))

# Notifications
noticeImportanceCutValue <- 0
notifications <- fetch(dbSendQuery(mydb, "SELECT * FROM notification WHERE timestamp >= CURDATE();"))
notifications <- notifications[notifications[,"importance"]>noticeImportanceCutValue,]

# Display info
print(paste("Nr of active tickers: ", nrActiveTickers, sep=""))
print(paste("Quote total: ", nrQuotesTotal, " Quote today: ", nrQuotesToday, sep=""))
print(paste("News total: ", nrNewsRecords, " News today: ", nrNewsRecordsToday, sep=""))
print(tail(notifications,15)

# Disconnect
dbFinDisconnect()

