## The rankhospital function ranks the hospitals in a state for a given outcome
## based on ascending mortality rate for that outcome, and returns the name of 
## the hospital with a specific ranking.
## state is the two letter uppercase abbreviation for a state.
## outcome is the health condtion of interest, it can be "heart attack", "heart failure", or "pneumonia"
## num is the ranking desired, possible inputs are "best", "worst", and a numerical value which corresponds to a specific ranking.
rankall <- function(outcome, num = "best") {
  
  # Tie conditioncol to appropriate column for given outcome
  codes <- c("heart attack" = 11, "heart failure" = 17, "pneumonia" = 23)
  conditioncol <- as.numeric(codes[outcome])
  
  # Check outcome is valid
  if(is.na(conditioncol)){
    conditioncol
    stop("invalid outcome")
    return(geterrmessage())    
  }
  
  # Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  suppressWarnings(data[,conditioncol] <- as.numeric(data[,conditioncol])) #warnings suppressed, coercing to numeric
  
  ## Return a data frame with the hospital names and the (abbreviated) state name
  data <- subset(data, !is.na(data[,conditioncol] == TRUE)) #cuts out na for condition
  data <- data[,c(2,7,conditioncol)]                    #cuts off everything but name, state, and conditioncol
  names(data)[3] <- "Rate"                            #renames conditioncol to "Rate"
  names(data)[2] <- "state"                            #renames state to "state"
  data <- data[order(data$state, data$Rate, data$Hospital.Name),] #sorts by state, rate, then name
  data$rank <- NA
  for(st in state.abb){
    numst <- sum(data$state == st)
    data[which(data$state == st),"rank"] <- 1:(numst)
  }

  # for(st in state.abb)
 #   data[,data$rank] <- 1:nrow(data$state = st)          
  
#   #return if statements
#   if(num == "best"){     #first entry
#     return(head(data$Hospital,1))
#   }
#   if(num == "worst"){    #last entry
#     return(tail(data$Hospital.Name,1))
#   } 
#   if(as.numeric(num) > nrow(data)){   #asking for nonexistent index
#     return(NA)
#   } else {               #normal return
#     return(data[num,1])
#   }
  
}