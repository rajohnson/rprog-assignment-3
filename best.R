## The 'best' function determines the best hospital in a given state for a given
## condition(outcome)
## state can be 2 letter uppercase abbreviation
## outcome can be "heart attack", "heart failure", or "pneumonia"
## In the event of a tie the first hospital is returned.
best <- function(state, outcome) {
  ## Read outcome data
  conditioncol <- as.numeric(c("heart attack" = 11, "heart failure" = 17, "pneumonia" = 23)[outcome])

  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  data[,conditioncol] <- as.numeric(data[,conditioncol])
  ## Check that state and outcome are valid
  if(!exists(state,where=state.abb)){
    stop("invalid state")
    return(geterrmessage())
  }
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  
}