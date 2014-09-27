best <- function(state, outcome) {
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

  ## Return hospital name in that state with lowest 30-day death rate
  dfram <- measures[ measures$State %in% state, c(2, which(names(measures) == i))]
  rownames(dfram) <- NULL
  dfram[ dfram == "Not Available" ] <- NA 
  dfram[order( as.numeric(dfram[,i]), dfram$Hospital.Name, na.last=TRUE) [1],][1]
}

