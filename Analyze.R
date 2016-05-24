
# Set working directory
# setwd("~/Projects/Fin/GitFin")

# Load packages
library(RMySQL)
library(reshape2)
library(ggplot2)
library(tm)
library(SnowballC)
library(gtools)
library(rpart)
library(randomForest)
library(RWeka)
library(beepr)
source("Source_Analysis_V005b.R")

if(file.exists("dbFunctions.R")) {
  source("dbFunctions.R")
  source("SourceTimeFunction.R") 
  source("SourceNewsFunctions.R")
  source("Source_Analysis_V005b.R")
  myHost <- "ec2-54-173-22-144.compute-1.amazonaws.com"
  myUser <- "remote"
  myPassword <- "nederland"
} else {
  setwd("/home/finance/GitFin/")
  source("/home/finance/GitFin/dbFunctions.R")
  source("/home/finance/GitFin/SourceTimeFunction.R")
  source("/home/finance/GitFin/SourceNewsFunctions.R")
  source("/home/finance/GitFin/Source_Analysis_V005b.R")
  myHost <- "localhost"
  myUser <- "finance"
  myPassword <- "nederland"
}


# Hi

# Function to run predictive model based on news data
PredictRF <- function() {
  
  # Load model
  load(file="Model_RF.RData")
  
  # Pull data
  # nData <- PullData("select * from basicnews order by newsID desc limit 100;")
  # tData <- PullData("SELECT * FROM ticker;")
  mydb = dbConnect(MySQL(), user='remote', password='nederland', host='ec2-54-173-22-144.compute-1.amazonaws.com')
  on.exit(dbDisconnect(mydb))
  rs <- dbSendQuery(mydb, "USE finance;")
  sTime <- Sys.time()
    nData <- dbFetch(dbSendQuery(mydb, "select * from basicnews order by newsID desc limit 100;"), n=-1)
    dbSendQuery(mydb, "UPDATE basicnews SET wasChecked=1")
    tData <- dbFetch(dbSendQuery(mydb, "SELECT * FROM ticker;"), n=-1)
  dbClearResult(dbListResults(mydb)[[1]])
  print(paste("Time taken:", round((as.numeric(Sys.time())-as.numeric(sTime))/60,2), " minutes"))
  
  # Add tickerID to news data and subset
  stData <- tData[tData$isActive==1,]
  row.names(stData) <- stData$symbol
  nData$tickerID <- stData[nData$tickerTags,"tickerID"]
  nData <- nData[!is.na(nData$tickerID),]; dim(nData)
  
  # Check for recent news 
  nData <- ProcessTimestamp(nData)
  nData <- nData[nData$dateTimeNum>=as.POSIXlt(Sys.time(), tz="America/New_York")-60*60*12,]
  nData <- nData[is.na(nData$wasChecked),]
  
  # Notify
  print(nrow(nData))
  if(nrow(nData)>0) {
    
    # Merge title and description
    nData$text <- paste(nData$title, nData$description)
    
    # Create corpus and term matrix
    fCorpus = Corpus(VectorSource(nData$text))
    fCorpus <- tm_map(fCorpus, removePunctuation)
    fCorpus <- tm_map(fCorpus, content_transformer(tolower))
    fCorpus <- tm_map(fCorpus, removeNumbers)
    fCorpus <- tm_map(fCorpus, stemDocument)
    fdtm <- DocumentTermMatrix(fCorpus); fdtm
    
    # Create Bigrams
    BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
    biGrams <- DocumentTermMatrix(fCorpus, control = list(tokenize = BigramTokenizer))
    
    # Create term matrix
    tMatrix <- as.matrix(fdtm); dim(tMatrix)
    
    # Find bigrams by frequencies
    biMatrix <- as.matrix(biGrams); dim(biMatrix)
    
    # Combine term and bigram matrices
    features <- c(fTerms, biTerms)
    tbiMatrix <- cbind(tMatrix, biMatrix)
    tbiMatrix <- tbiMatrix[,colnames(tbiMatrix) %in% features]; dim(tbiMatrix)
    
    # Create and populate feature matrix
    featureMatrix <- matrix(0,nrow=nrow(tMatrix), ncol=length(features), dimnames=list(rownames(tMatrix),features))
    for(tag in colnames(tbiMatrix)) { featureMatrix[,tag] <- tbiMatrix[,tag] }
    
    # Run prediction
    prediction <- predict(rf, newdata=featureMatrix)
    nData <- cbind(nData, prediction)
    # plot(prediction)
    
    # Compare to cutoff
    #cutoff <- 1.0225
    cutoff <- 1.01
    sData <- nData[nData$prediction>=cutoff,c("tickerID", "timestamp", "prediction","text")]
    if(nrow(sData)>0) { 
      print(sData)  
      # NotifyBeep(n=nrow(sData), sound="treasure")
      return(sData)  
    } else {
      return(NA)
    }
    
  } else {
    return(NA)
  }
}

# Run prediction
prediction <- PredictRF()

# Return result
if(is.na(prediction)==FALSE) { 
  
  # Print
  print(prediction) 
  
  # Add to notification
  for(rNr in 1:nrow(prediction)) {
    # Add message
    message <- paste(prediction[rNr,c("tickerID", "timestamp", "prediction")], collapse=" ")
    dbNotification(message, 50)
  }
  
  
  
  # Send mail  
  
}



