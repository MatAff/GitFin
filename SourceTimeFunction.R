
# Load packages
library(lubridate)

# Function to process time information
GetTime <- function(dateTimeText) {
  
  # Get year
    year <- unlist(regmatches(dateTimeText, gregexpr("201[0-9]", dateTimeText)))
    dateTimeText <- sapply(1:length(dateTimeText), function(x) gsub(year[x], "", dateTimeText[x]))
    
  # Get weekday
    daysVec <- c("mon|tue|wed|thu|fri|sat|sun|Mon|Tue|Wed|Thu|Fri|Sat|Sun")
    weekDay <- unlist(regmatches(dateTimeText, gregexpr(daysVec, dateTimeText)))
    dateTimeText <- sapply(1:length(dateTimeText), function(x) gsub(weekDay[x], "", dateTimeText[x]))
  
  # Get time zone
    timeZone <- unlist(regmatches(dateTimeText, gregexpr("[A-Z]{3}|-[0-9]{2}00", dateTimeText)))
    dateTimeText <- sapply(1:length(dateTimeText), function(x) gsub(timeZone[x], "", dateTimeText[x]))
  
  # Get time section
    timeText <- unlist(regmatches(dateTimeText, gregexpr("[0-2][0-9]:[0-5][0-9]:[0-5][0-9]", dateTimeText)))
    dateTimeText <- sapply(1:length(dateTimeText), function(x) gsub(timeText[x], "", dateTimeText[x]))
  
  # Get month tag
    month <- regmatches(dateTimeText, gregexpr("[a-zA-Z]{3}", dateTimeText))
    monthTags <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
    monthNrs <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")
    for(m in 1:12) { month <- sapply(1:length(month), function(x) gsub(monthTags[m], monthNrs[m], month[x])) }
    dateTimeText <- sapply(1:length(dateTimeText), function(x) gsub(month[x], "", dateTimeText[x]))
  
  # Date
    date <- regmatches(dateTimeText, gregexpr(" [0-9]{2} ", dateTimeText))
    date <- gsub(" ", "", date)
    dateTimeText <- sapply(1:length(dateTimeText), function(x) gsub(date[x], "", dateTimeText[x]))
  
  # Compile date string
    dateTimeText <- paste(year,"-",month,"-",date, " ", timeText, sep="")
  
  # Apply correction
    timeZoneText <- timeZone
    timeZoneCorrection <- as.numeric(regmatches(timeZone, gregexpr("-[0-9]{2}00", timeZone)))/100
    timeZoneCorrection[is.na(timeZoneCorrection)] <- 0
    timeZoneText[timeZoneCorrection!=0] <- "UTC"
    timeZoneText <- gsub("EDT", "America/New_York", timeZoneText)
    dateTimeText <- ymd_hms(dateTimeText) -  hours(timeZoneCorrection)
  
  # Overwrite incorrectly assumed timezones  
    dateTimeTextTZ <- rep(NA,length(dateTimeText))  
    for(x in 1:length(dateTimeText)) {
      temp <- force_tz(dateTimeText[x], timeZoneText[x])
      dateTimeTextTZ[x] <- format(temp, tz="America/New_York",usetz=TRUE)
    }
   
  # Return dateTimeText
    return(dateTimeTextTZ)
  
}

# Test dates
#dateTimeText <- c("Sat, 01 Aug 2015 14:59:01 -0400", "Sat, 01 Aug 2015 18:45:03 GMT", "Sat, 01 Aug 2015 14:01:46 EDT" )
#GetTime(dateTimeText)


