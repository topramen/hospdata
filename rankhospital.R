rankhospital <- function(state, outcome, num ="best") {
  ## Read outcome data
  measures <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that state and outcome are valid
  states <- unique(measures$State)
  outcomes <- c("heart attack", "heart failure", "pneumonia")
    if (!(state %in% states)) {
    stop("invalid state")
    }
  switch(outcome,
         "heart attack" = {i <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"},
         "heart failure" = {i <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"},
         "pneumonia" = {i <- "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"},
         stop("invalid outcome"))

  dfram <- measures[ measures$State %in% state, c(2, which(names(measures) == i))] 
  rownames(dfram) <- NULL
  na.l = TRUE
  if (num == "best") {
    num <- 1 
  } else if (num == "worst" ) {
    num <- nrow(dfram)
    na.l = FALSE
  }  

  ## Return hospital name in that state with lowest 30-day death rate

  dfram[ dfram == "Not Available" ] <- NA 
  dfram[order( as.numeric(dfram[,i]), dfram$Hospital.Name, na.last=na.l) [num],][1]
  
}

