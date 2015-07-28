
# Load packages
library(XML)

# Get source
url <- "http://stackoverflow.com/feeds"
url <- "http://www.fool.com/feeds/index.aspx?id=foolwatch&format=rss2"
con = url(url); htmlCode = readLines(con); close(con)
xmlfile <- xmlTreeParse(htmlCode)
xmltop = xmlRoot(xmlfile)
xmltop[1:5]

# Get subnodes
subnode <- getNodeSet(xmltop, "//channel/item")

# Function to extract data from node
getDetails <- function(nodes) {
  
  # Process data
  nData <- xmlSApply(nodes, function(x) xmlSApply(x, xmlValue))
  
  # Get elements
  timeStamp <- nData["pubDate.text"]
  site <- "fool.com"
  title <- nData["title.text"]
  description <- nData["description"]
  link <- nData["guid.text"]
  
  # Tickers
  tData <- as.data.frame(nData, stringsAsFactors=FALSE)
  tickers <- grep("^([A-Z]{1,5})$", tData[,1], value=TRUE)
  tickers <- unique(tickers)
  tickers <- paste(tickers,collapse=",")
  
  # Create data frame to return
  returnData <- data.frame(timeStamp, site, title, description, link, tickers, row.names = NULL)
  
  # Return data frame
  return(returnData)
  
}

# Loop through list to pull data and organize
rData <- lapply(subnode, getDetails)
rData <- as.data.frame(do.call(rbind, rData))

# Display data
print(rData)

# Write to database
library(RMySQL)

# Set up connection
mydb = dbConnect(MySQL(), user='finance', password='nederland', host='localhost')
on.exit(dbDisconnect(con))

# Select database
rs <- dbSendQuery(mydb, "USE finance;")

# Enter data
for(rowNr in 1:nrow(rData)) {
  query <- paste("INSERT INTO news (n_timestamp, site, title, description, link, tickers) 
          VALUES ('", rData[rowNr, "timeStamp"], "', '", rData[rowNr, "site"], 
        "', '", rData[rowNr, "title"], "', '", rData[rowNr, "description"]
        , "', '", rData[rowNr, "link"], "', '", rData[rowNr, "tickers"], "');", sep="")
  print(query)
  rs <- dbSendQuery(mydb, query)
}

# Disconnect
dbDisconnect(mydb)
