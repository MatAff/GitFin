
# Load packages
  library(quantmod)

# Input - Load symbols - Hardcoded
  selectSymbols <- c("AAPL", "ADBE", "ADI", "ADP", "ADSK", "AKAM", "ALTR", "ALXN", "AMAT", "AMGN", "AMZN", 
    "ATVI", "AVGO", "BBBY", "BIDU", "BIIB", "BRCM", "CA", "CELG", "CERN", "CHKP", "CHRW", 
    "CHTR", "CMCSA", "COST", "CSCO", "CTRX", "CTSH", "CTXS", "DISCA", "DLTR", "DTV", "EBAY", 
    "EQIX", "ESRX", "EXPD", "EXPE", "FAST", "FB", "FFIV", "FISV", "FOSL", "FOXA", "GILD", 
    "GMCR", "GOLD", "GOOG", "GRMN", "HSIC", "INTC", "INTU", "ISRG", "KLAC", "KRFT", "LBTYA", 
    "LINTA", "LLTC", "LMCA", "MAT", "MCHP", "MDLZ", "MNST", "MSFT", "MU", "MXIM", "MYL", 
    "NFLX", "NTAP", "NUAN", "NVDA", "ORLY", "PAYX", "PCAR", "PCLN", "QCOM", "REGN", "ROST", 
    "SBAC", "SBUX", "SHLD", "SIAL", "SIRI", "SNDK", "SPLS", "SRCL", "STX", "SYMC", "TSLA", 
    "TXN", "VIAB", "VOD", "VRSK", "VRTX", "WDC", "WFM", "WYNN", "XLNX", "XRAY", "YHOO")

# Subset for development purposes
  selectSymbols <- selectSymbols[1:5]

# Processing - Get quote
  quoteData <- getQuote(selectSymbols)

# Output - Print data to screen - For dev purposes
  for(shareNr in 1:length(selectSymbols)) {
    print(paste(selectSymbols[shareNr], " - ", quoteData[shareNr, "Last"], sep=""))
  }

# Write to database
  library(RMySQL)

# Set up connection
  mydb = dbConnect(MySQL(), user='finance', password='nederland', host='localhost')
  on.exit(dbDisconnect(con))

# Select database
  rs <- dbSendQuery(mydb, "USE finance;")

# Enter data
  query <- paste("INSERT INTO prices (ticker, price)
    VALUES ('TEST', '99.99');")
  rs <- dbSendQuery(mydb, query)

# Disconnect
  dbDisconnect(mydb)