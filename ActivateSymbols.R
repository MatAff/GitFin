
# Load packages
  library(RMySQL)
  
# Select symbols
  activate <- c("AAPL", "ADBE", "ADI", "ADP", "ADSK", "AKAM", "ALTR", "ALXN", "AMAT", "AMGN", "AMZN", 
                "ATVI", "AVGO", "BBBY", "BIDU", "BIIB", "BRCM", "CA", "CELG", "CERN", "CHKP", "CHRW", 
                "CHTR", "CMCSA", "COST", "CSCO", "CTRX", "CTSH", "CTXS", "DISCA", "DLTR", "DTV", "EBAY", 
                "EQIX", "ESRX", "EXPD", "EXPE", "FAST", "FB", "FFIV", "FISV", "FOSL", "FOXA", "GILD", 
                "GMCR", "GOLD", "GOOG", "GRMN", "HSIC", "INTC", "INTU", "ISRG", "KLAC", "KRFT", "LBTYA", 
                "LINTA", "LLTC", "LMCA", "MAT", "MCHP", "MDLZ", "MNST", "MSFT", "MU", "MXIM", "MYL", 
                "NFLX", "NTAP", "NUAN", "NVDA", "ORLY", "PAYX", "PCAR", "PCLN", "QCOM", "REGN", "ROST", 
                "SBAC", "SBUX", "SHLD", "SIAL", "SIRI", "SNDK", "SPLS", "SRCL", "STX", "SYMC", "TSLA", 
                "TXN", "VIAB", "VOD", "VRSK", "VRTX", "WDC", "WFM", "WYNN", "XLNX", "XRAY", "YHOO")
  
  deactivate <- c("VOD")

# Check and print overlapping symbols
  overlap <- intersect(activate, deactivate)
  if(length(overlap)>0) {
    print(paste("Symbols appear in activate and deactivate list:", overlap))
  }
    
# Set up connection
  mydb = dbConnect(MySQL(), user='finance', password='nederland', host='localhost')
  on.exit(dbDisconnect(con))
  
# Query
  rs <- dbSendQuery(mydb, "USE finance;")  
  
  # Active
  if(length(activate)!=0) {
    activate <- paste(c("'",paste(activate, collapse="', '"),"'"), collapse="")
    print(paste("Activating: ", activate, sep=""))
    query <- paste("UPDATE ticker SET isActive = 1 WHERE symbol in (", activate, ");", sep="")
    rs <- dbSendQuery(mydb, query)  
    
  }
  
  # Deactive
  if(length(deactivate)!=0) {
    deactivate <- paste(c("'",paste(deactivate, collapse="', '"),"'"), collapse="")
    print(paste("Deactivating: ", activate, sep=""))
    query <- paste("UPDATE ticker SET isActive = 0 WHERE symbol in (", deactivate, ");", sep="")
    rs <- dbSendQuery(mydb, query)  
  }
  
# Disconnect
  dbDisconnect(mydb)
  