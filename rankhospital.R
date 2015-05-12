rankhospital <- function(state, outcome, num = "best") {
  
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
  
  ## Return hospital name in that state with the given rank & 30-day death rate
  data <- subset(data, data[,7] == state)        #trims set for state
  data <- subset(data, is.na(data[,conditioncol] == TRUE)) #cuts out na for condition
  
}