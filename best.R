## The 'best' function determines the best hospital in a given state for a given
## condition(outcome)
## state can be 2 letter uppercase abbreviation
## outcome can be "heart attack", "heart failure", or "pneumonia"
## In the event of a tie the first hospital is returned.
best <- function(state, outcome) {
  
  # Tie conditioncol to appropriate column for given outcome
  codes <- c("heart attack" = 11, "heart failure" = 17, "pneumonia" = 23)
  conditioncol <- as.numeric(codes[outcome])
  
  # Check that state and outcome are valid
  if(!is.element(state, state.abb)){
    stop("invalid state")
    return(geterrmessage())
  }
  if(is.na(conditioncol)){
    stop("invalid outcome")
    return(geterrmessage())    
  }
  
  # Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  suppressWarnings(data[,conditioncol] <- as.numeric(data[,conditioncol])) #warnings suppressed, coercing to numeric

  # Return hospital name in that state with lowest 30-day death rate
  data <- subset(data, data[,7] == state)
  minval <- min(data[,conditioncol], na.rm=TRUE)
  data[which(data[,conditioncol] == minval),2]
}