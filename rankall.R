library(plyr)

rankinstate <- function (hosp, meas, stat) {
  na.l <<- TRUE
  if (num == "best") {
    num <<- 1 
  } else if (num == "worst" ) {
    num <<- nrow(hosp)
    na.l <<- FALSE
  } else num <<- num
  
  ## Return hospital name in that state with lowest 30-day death rate
  
  meas[ meas == "Not Available" ] <- NA 
  result <- order( as.numeric(meas), hosp, stat, na.last=na.l)
  return (hosp[result[num]])
}


rankall <- function(outcome, num ="best") {
  ## Read outcome data
  measures <<- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that state and outcome are valid
  states <- states[order(unique(measures$State))]
  outcomes <- c("heart attack", "heart failure", "pneumonia")
  
  switch(outcome,
         "heart attack" = {i <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"},
         "heart failure" = {i <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"},
         "pneumonia" = {i <- "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"},
         stop("invalid outcome"))
#   na.l <<- TRUE
#   if (num == "best") {
#     num <<- 1 
#   } else if (num == "worst" ) {
#     num <<- nrow(hosp)
#     na.l <<- FALSE
#   } else num <<- num
  ddply(measures,"State",function(x) rankinstate (x[,2], x[,which(names(x) == i)], x[,7]) )
  
}

