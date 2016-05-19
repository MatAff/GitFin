
# Function to pull data based on SQL query
PullData <- function(sqlQuery) {
  # This currently doesn't not close the connection which is a problem, most likely
  mydb = dbConnect(MySQL(), user='remote', password='nederland', host='ec2-54-173-22-144.compute-1.amazonaws.com')
  on.exit(dbDisconnect(mydb))
  rs <- dbSendQuery(mydb, "USE finance;")
  sTime <- Sys.time()
  theData <- dbFetch(dbSendQuery(mydb, sqlQuery), n=-1)
  dbClearResult(dbListResults(mydb)[[1]])
  print(paste("Time taken:", round((as.numeric(Sys.time())-as.numeric(sTime))/60,2), " minutes"))
  #dbDisconnect(mydb)
  return(theData)
}

# Function to get numerical value for trading days
GetTradingDays <- function(sDate="2015-01-01") {
  
  # Convert timestamp to POSIX
  #if(class(sDate)=="character") { sDate <- as.POSIXct(sDate,tz="America/New_York") }
  if(class(sDate)=="character") { sDate <- as.Date(sDate,tz="America/New_York") }
  
  # Get date vec
  dateVec <- sDate:(sDate+1999)
  dateVec <- as.Date(dateVec, origin = as.POSIXct("1970-01-01", tz = "America/New_York"))
  dateVec <- dateVec[!grepl("S(at|un)", weekdays(dateVec))]
  
  # Create data frame and return
  tradingDays <- data.frame(row.names = as.numeric(dateVec), dateChar=as.character(dateVec), date=dateVec, tDayNr=c(1:length(dateVec)))
  return(tradingDays)
  
}

# Function to process time info into more usable format
ProcessTimestamp <- function(pData, preview=FALSE) {
  
  pData[,"POSIX"] <- as.POSIXct(pData[,"timestamp"],tz="America/New_York")                     # Convert timestamp to POSIX
  pData[,"date"] <- as.Date(as.POSIXct(pData[,"timestamp"]))                                   # Seperate date
  pData[,"dateNum"] <- as.numeric(pData[,"date"])                                              # Create numeric date
  pData[,"dateChar"] <- as.character(pData[,"date"])                                           # Create character data
  pData[,"time"] <- format(as.POSIXct(pData[,"timestamp"]), format="%H:%M:%S")                 # Seperate time
    hours <- as.numeric(format(as.POSIXct(pData[,"timestamp"]), format="%H"))
    hours[hours<5] <- hours[hours<5] + 12
    minutes <- as.numeric(format(as.POSIXct(pData[,"timestamp"]), format="%M"))
  pData[,"decTime"] <- hours + round(minutes/15,0)/4                                           # Create decimal time
  pData[,"ticDate"] <- pData[,"tickerID"] * 10 ^ 5 + as.numeric(pData[,"date"])                # Combine tickerID and numeric date
  pData[,"dateTimeNum"] <- as.numeric(as.POSIXct(pData[,"timestamp"],tz="America/New_York"))   # Numeric date and time 
  
  # Add trading day nrs and time where opening = 0 and closing = 1
  tradingDays <- GetTradingDays()
  pData[,"tDayNr"] <- tradingDays[as.character(pData$dateNum), "tDayNr"]
  pData[,"tTime"] <- (pData$decTime-9.5)/(16-9.5)/100*99
  pData[,"tDayTime"] <- pData$tDayNr + pData$tTime
  
  # Preview and return data
  if(preview) { print(head(pData)); print(dim(pData)) }
  return(pData)
  
}

# Function to get ratio of change over days
OutcomeDayRatioSingle <- function(tickerID, timestamp, pData) {
  
  # Convert timestamp to POSIX
  if(class(timestamp)=="character") { timestamp <- as.POSIXct(timestamp,tz="America/New_York") }
  
  # Create ratio list
  ratioData <- matrix(NA, nrow=1, ncol=6,dimnames = list(c(),c(paste("D",0:5,sep=""))))
  
  # Create dateVec (removing weekend days)
  dateVec <- as.Date(timestamp,tz="America/New_York")
  for(j in 1:7) { dateVec <- c(dateVec,dateVec[1]+j) }
  dateVec <- dateVec[c(TRUE,!grepl("S(at|un)", weekdays(dateVec[c(2:length(dateVec))])))]
  
  # Get price data
  sData <- GetPriceData(tickerID=tickerID, dateVec=dateVec, pData=pData)
  postData <- sData[sData$POSIX>timestamp,]
  if(nrow(postData)>0) { startPrice <- postData[1,"price"] } else { startPrice <- NA }
  postData$ratio <- postData$price/startPrice
  
  # Aggregate
  aData <- postData[,c("dDate", "ratio")] 
  if(nrow(aData)>0) {
    aData <- aggregate(aData, by=list(aData$dDate), mean)
    aData <- data.frame(row.names=aData$dDate, ratio=aData$ratio)
    aData <- t(aData)
    aData <- as.data.frame(aData)
    names(aData) <- paste("D", names(aData),sep="")
    aData
  } else {
    aData <- data.frame(D0=NA)
  }
  
  for(cTag in names(aData)){ ratioData[1,cTag] <- aData[1,cTag] }
  
  return(ratioData)
  
}

# Function to get ratio of change over days
OutcomeDayRatioMult <- function(x) { 
  OutcomeDayRatioSingle(tickerIDVec[x], timestampVec[x], pData) 
}

# Function to get ratio of change over days
OutcomeDayRatio <- function(tickerIDVec, timestampVec, pData, multicore=FALSE) {
  
  # Convert timestamp to POSIX
  if(class(timestampVec)=="character") { timestampVec <- as.POSIXct(timestampVec,tz="America/New_York") }
  
  if(multicore==FALSE) {
    
    Ratio.List <- vector("list", length(tickerIDVec))
    for(rNr in 1:length(Ratio.List)) {
      Ratio.List[[rNr]] <- OutcomeDayRatioSingle(tickerID=tickerIDVec[rNr], timestamp=timestampVec[rNr], pData)
    }
    
  } else {
    
    cores=detectCores()
    cl <- makeCluster(cores)
    clusterExport(cl=cl, c("OutcomeDayRatioSingle", "tickerIDVec", "timestampVec", "pData", "GetPriceData"))
    Ratio.List <- clusterApply(cl=cl, x=1:length(tickerIDVec), fun=OutcomeDayRatioMult)
    stopCluster(cl)
    
  }
  
  ratioData <- do.call("smartbind", Ratio.List)
  return(ratioData)
  
}

# Function to get ratio of change over days
OutcomeDayRatioOld <- function(tickerIDVec, timestampVec, pData) {
  
  # Convert timestamp to POSIX
  if(class(timestampVec)=="character") { timestampVec <- as.POSIXct(timestampVec,tz="America/New_York") }
  
  # Create ratio list
  ratioData <- matrix(NA, nrow=length(tickerIDVec), ncol=6,
                      dimnames = list(c(),c(paste("D",0:5,sep=""))))
  
  # Loop through vectors
  for(i in 1:length(tickerIDVec)) {
    
    # Create dateVec (removing weekend days)
    dateVec <- as.Date(timestampVec[i],tz="America/New_York")
    for(j in 1:7) { dateVec <- c(dateVec,dateVec[1]+j) }
    dateVec <- dateVec[c(TRUE,!grepl("S(at|un)", weekdays(dateVec[c(2:length(dateVec))])))]
    
    # Get price data
    sData <- GetPriceData(tickerID=tickerIDVec[i], dateVec=dateVec, pData=pData)
    postData <- sData[sData$POSIX>timestampVec[i],]
    if(nrow(postData)>0) { startPrice <- postData[1,"price"] } else { startPrice <- NA }
    postData$ratio <- postData$price/startPrice
    
    # Aggregate
    aData <- postData[,c("dDate", "ratio")] 
    if(nrow(aData)>0) {
      aData <- aggregate(aData, by=list(aData$dDate), mean)
      aData <- data.frame(row.names=aData$dDate, ratio=aData$ratio)
      aData <- t(aData)
      aData <- as.data.frame(aData)
      names(aData) <- paste("D", names(aData),sep="")
      aData
    } else {
      aData <- data.frame(D0=NA)
    }
    
    for(cTag in names(aData)){ ratioData[i,cTag] <- aData[1,cTag] }
    
  }
  
  return(ratioData)
  
}

# Get price data
GetPriceData <- function(tickerID, dateVec, pData) {
  
  # Subset based on tickerID
  sData <- pData[pData$tickerID==tickerID,]
  
  # Subset based on dateVec
  sData <- sData[sData$date %in% dateVec,] 
  
  # Add delta date number
  deltaData <- data.frame(row.names = dateVec,d=c(1:length(dateVec))-1)
  sData["dDate"] <- deltaData[sData$dateChar,"d"]
  
  return(sData)
}

# Split by date chunks
SplitByDate <- function(dateVec, n, prob, chunkSize, seed) {
  set.seed(seed)
  dateNrs <- as.numeric(dateVec)
  chunkNrs <- cut(dateNrs,floor(length(dateNrs)/chunkSize), labels=FALSE) 
  uniChunkNrs <- unique(chunkNrs)
  indVal <- sample(n, length(uniChunkNrs), replace=TRUE, prob=prob)
  assignData <-data.frame(row.names=uniChunkNrs,indVal)
  ind <- assignData[as.character(chunkNrs),1]
  return(ind)
}

# Function to create and clean corpus
CreateCorpus <- function(textVec, MyDictionary=NULL) {
  fCorpus = Corpus(VectorSource(textVec))
  fCorpus <- tm_map(fCorpus, removePunctuation)
  fCorpus <- tm_map(fCorpus, content_transformer(tolower))
  fCorpus <- tm_map(fCorpus, removeNumbers)
  fCorpus <- tm_map(fCorpus, removeWords, row.names(MyDictionary)[MyDictionary$Remove==TRUE])
  fCorpus <- tm_map(fCorpus, stemDocument)
  return(fCorpus)
}

# BigramTokenizer
BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))

# Function to check model performance
CheckModelPerformance <- function(model, buildData, testData, features, outcome, cutoff) {
  # Ideally 
  # Plot showing effect of different cutoffs
  # Plot showing effect different cutoff ratios 
  # Plot mean day effect by news items
  # Plot high-resolution news effect
  
  # Add predictions and categorizations to data
  buildData$Predict <- predict(rf)
  buildData$Actual <- buildData[,outcome]
  buildData$Category <- buildData$Predict > cutoff
  testData$Predict <- predict(rf, newdata=testData[,features])
  testData$Actual <- testData[,outcome]
  testData$Category <- testData$Predict > cutoff

  # Plots
  p1 <- PerfScatter(buildData, cutoff, "Build Data")
  p2 <- PerfDensity(buildData, "Build Data")
  p3 <- PerfScatter(testData, cutoff, "Test Data")
  p4 <- PerfDensity(testData, "Test Data")
  p5 <- PerfDayMean(testData)
  p6 <- PerfRatioLine(testData)
  
  # Combine plots
  multiplot(p1,p3,p2,p4,p5,p6, cols=3)
  return(list(p1,p3,p2,p4,p5,p6))
  
}

# Performance plot scatter plot
PerfScatter <- function(theData, cutoff, title) {
  return(ggplot(theData, aes(x=Predict,y=Actual)) + geom_point() + geom_vline(xintercept=cutoff) + geom_hline(yintercept=1.01) + ggtitle(title))
}

# Performance plot density
PerfDensity <- function(theData, title) {
  trueMean
  falseMean 
  
  return(ggplot(theData, aes(x=Actual, group=Category, color=Category)) + geom_density() + ggtitle(title))
}

# Performance plot day mean line
PerfDayMean <- function(theData) {
  theData <- theData[theData$Category==TRUE,grep("^D[0-9]",names(trueData),value=TRUE)]
  ggData <- melt(as.matrix(theData))
  names(ggData) <- c("ID", "D", "Ratio")
  ggData$ID <- as.factor(ggData$ID)
  return(ggplot(ggData, aes(x=D, y=Ratio, group=ID, color=ID)) + geom_line() + theme(legend.position="none"))
}

# Performance plot line ratio
PerfRatioLine <- function(theData) {
  
  # Select true data
  theData <- theData[theData$Category==TRUE,]
  
  # Line plot all data
  A.List <- vector("list",nrow(theData))
  for(tNr in 1:length(A.List)) {
    
    # Get IDs
    newsID <- theData[tNr,"newsID"]
    tickerID <- theData[tNr,"tickerID"]
    timeStamp <- theData[tNr,"timestamp"]
    dateTimeNum <- as.numeric(as.POSIXct(timeStamp,tz="America/New_York"))
    
    # Select data
    selectData <- pData[pData$tickerID==tickerID,]
    selectData <- selectData[selectData$dateTimeNum>=dateTimeNum,]
    selectData <- selectData[selectData$dateTimeNum<(dateTimeNum+7*24*60*60),]
    selectData <- selectData[,c("dateTimeNum","price","tickerID","tDayTime")]
    selectData <- selectData[order(selectData$tDayTime),]
    
    # Massage data
    selectData$tDayTime <- selectData$tDayTime-min(selectData$tDayTime)
    selectData$price <- selectData$price/selectData[1,"price"]
    if(nrow(selectData)>0) { selectData$newsID <- newsID }
    
    # Add to list
    A.List[[tNr]] <- selectData
    
  }
  
  # Merge and plot
  ggData <- do.call(smartbind, A.List); head(ggData); dim(ggData)
  ggData$newsID <- as.factor(ggData$newsID)
  p <- ggplot(ggData, aes(x=tDayTime, y=price, group=newsID, color=newsID)) + geom_line() +
    theme(legend.position="none")
  return(p)
  
}

# Function to get price based on ticker id and dateTime
GetPrice <- function(tickerID, dateTimeVec, pData, margin=20) {
  
  # If the dateTimeVec is not a number convert
  if(class(dateTimeVec)!="numeric") { dateTimeVec <- as.numeric(as.POSIXct(dateTimeVec,tz="America/New_York")) } 
  
  # Subset ticker data  
  sData <- pData[pData$tickerID==tickerID,]
  
  # Compute time differences
  timeDiff.List <- lapply(dateTimeVec, function(x) abs(sData$dateTimeNum - x))
  
  # Get min time difference
  minVec <- unlist(lapply(timeDiff.List, min))
  minVec[minVec>margin*60] <- NA
  
  # Get prices
  priceVec <- unlist(sapply(1:length(dateTimeVec), function(x) sData[timeDiff.List[[x]]==minVec[x],"price", drop=FALSE][1,]))
  
  # Return prices
  return(priceVec)
}

# Function to convert quote list to day matrix/data.frame
GetDayMatrix <- function(pData) {
  
  uniqueTicDate <- unique(pData[,"ticDate"])
  uniqueTimes <- unique(pData[,"decTime"])
  uniqueTimes <- uniqueTimes[order(uniqueTimes)]
  
  pMat <- matrix(NA, nrow=length(uniqueTicDate), ncol=length(uniqueTimes))
  rownames(pMat) <- uniqueTicDate; colnames(pMat) <- uniqueTimes
  
  # RESOURCE SUCKER RIGHT HERE! FIX LATER
  for(rNr in 1:nrow(pData)) {
    pMat[as.character(pData[rNr,"ticDate"]),as.character(pData[rNr,"decTime"])] <- pData[rNr,"price"]
  }
  
  return(pMat)
  
}

# Function give status report of dayMatrix (missing, days covered etc)
StatusReportDayMat <- function(dayMatrix) {
  
  # Remove columns data don't have numeric name (in case we added other info already)
  cleanDayMatrix <- dayMatrix[,!is.na(as.numeric(colnames(dayMatrix)))]
  
  # Compute some info
  totalRows <- nrow(cleanDayMatrix)
  totalCols <- ncol(cleanDayMatrix)
  potentialDataPoints <- totalRows * totalCols
  totalMissing <- sum(is.na(cleanDayMatrix))
  percentageMissing <- totalMissing/potentialDataPoints * 100 
  missingByRow <- sapply(1:nrow(cleanDayMatrix), function(x) sum(is.na(cleanDayMatrix[x,])))
  
  # Print some info to screen
  print(paste("Total Rows:", totalRows))
  print(paste("Potential data points:", potentialDataPoints))
  print(paste("Total missing:", totalMissing))
  print(paste("Percentage missing:", round(percentageMissing,1)))
  print(paste("Disribution of missing:"))
  table(missingByRow)
  
}

# Function to populate missing values where possible 
FixMissing <- function(dayMatrix, maxMissing=5, start=9.5, end=16) {
  
  # Remove columns outside trading hours
  colValues <- as.numeric(colnames(dayMatrix))
  dayMatrix <- dayMatrix[,((colValues>=start) + (colValues<=end))==2]
  
  # Remove all cases with too many missing
  missingByRow <- sapply(1:nrow(dayMatrix), function(x) sum(is.na(dayMatrix[x,])))
  dayMatrix <- dayMatrix[missingByRow <= maxMissing, ]
  
  # If first of last are missing forget it
  dayMatrix <- dayMatrix[!is.na(dayMatrix[,1]),]
  dayMatrix <- dayMatrix[!is.na(dayMatrix[,"16"]),]
  
  # Find out which rows to fix
  missingByRow <- sapply(1:nrow(dayMatrix), function(x) sum(is.na(dayMatrix[x,])))
  rowsToFix <- c(1:length(missingByRow))[missingByRow>0]
  
  # Fix rows
  for(rNr in rowsToFix) {
     
    colsToFix <- c(1:ncol(dayMatrix))[is.na(dayMatrix[rNr,])]
 
    for(cNr in colsToFix) {
 
      lastKnown <- max(c(1:cNr)[!is.na(dayMatrix[rNr,1:cNr])])
      nextKnown <- min(c(cNr:ncol(dayMatrix))[!is.na(dayMatrix[rNr,cNr:ncol(dayMatrix)])])
      val <- (cNr-lastKnown)/(nextKnown-lastKnown) * (dayMatrix[rNr,nextKnown] - dayMatrix[rNr,lastKnown]) + dayMatrix[rNr,lastKnown]
      dayMatrix[rNr,cNr] <- val
    }
    
  }

  return(dayMatrix)

}

AddDateToDayMatrix <- function(dayMatrix) {
  date <- as.numeric(row.names(dayMatrix))-round(as.numeric(row.names(dayMatrix))/10^5,0)*10^5
  dayMatrix <- cbind(date, dayMatrix)
  return(dayMatrix)
}

# Function to compute rates of return
GetRatio <- function(dayMatrix, colName) {
  cNames <- colnames(dayMatrix)
  denom <- dayMatrix[,colName]
  timeNames <- as.character(seq(9.5,16,0.25))
  dayMatrix[,timeNames] <- sapply(1:length(timeNames), function(x) dayMatrix[,timeNames[x]]/denom)
  colnames(dayMatrix) <- cNames
  return(dayMatrix)
}

headCorpus <- function(theCorpus, n=15) { for(lNr in 1:n) { print(as.character(theCorpus[[lNr]])) } }

# Multiple plot function
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

# Notify function
NotifyBeep <- function(n=1, sound="ping") {
  if(n>=1) {
    for(i in 1:n) {
      beep(sound=sound)
      Sys.sleep(0.5)
    }
  }
}





