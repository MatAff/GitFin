#####################
### Load packages ###
#####################

library(quantmod)
library(RMySQL)
library(sendmailR)

if(file.exists("dbFunctions.R")) {
  source("dbFunctions.R")
} else {
  source("/home/finance/GitFin/dbFunctions.R")  
}

#################################
### ANALYZE PREDICT FUNCTIONS ###
#################################

# Analyze Predict Functions go here
PredictDropRecover <- function() {
  
}

# Predict based on news
PredictNews <- function() {
  
}

#######################
### RUN PREDICTIONS ###
#######################

# Create empty prediction object
predictions <- data.frame()

# Drop recover
dataToAdd <- PredictDropRecover()
predictions <- rbind(predictions, dataToAdd)

# News
dataToAdd <- PredictNews()
predictions <- rbind(predictions, dataToAdd)

# Predictions
# tickerID
# ticker
# current price
# targe price
# associated image

###############################
### COMMUNICATE PREDICTIONS ###
###############################

# Email


# Delete images




