
# Load packages
  source("dbFunctions.R")

########################
### ADD NOTIFICATION ###
########################

  noticeText <- "This is a test"
  dateTimeText <- as.POSIXlt(Sys.time(), "America/New_York") 

  dbFinConnect()
  dbNotification(noticeText, dateTimeText, 1)
  dbFinDisconnect()

  
  
  
  
  
  