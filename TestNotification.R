
# Load packages
  source("dbFunctions.R")

########################
### ADD NOTIFICATION ###
########################

  noticeText <- "This is a test 2"
  
  #mydb = dbConnect(MySQL(), user='finance', password='nederland', host='localhost')
  #on.exit(dbDisconnect(mydb))
  #rs <- dbSendQuery(mydb, "USE finance;")
  
  dbFinConnect()
  dbNotification(dateTimeText, noticeText, 1)
  dbFinDisconnect()

  
  
  
  
  
  