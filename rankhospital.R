rankhospital <- function(state, outcome, num)
{
  ## Read outcome data
  data <- read.csv("data/outcome-of-care-measures.csv", colClasses = "character")
 
  ## Check that state and outcome are valid
  if(! outcome %in% c("heart attack", "heart failure", "pneumonia"))
    stop("invalid outcome")
  
  if(! state %in% data$State)
    stop("invalid state")
  
  ## Return hospital name in that state with the given rank 30-day death rate
  column <- 
    if( outcome == "heart attack") { 11 }
  else if( outcome == "heart failure") { 17 }
  else { 23 }
  
  d1   <- data[data$State == state,]
  ## check if num is larger than the number of hospitals in that state
  if( is.numeric(num) && nrow(d1) < num)
    return(NA)
  
  ## clean non numeric data
  idx1 <- d1[, column] != "Not Available"
  ## get rate and hospitals
  rate      <- as.numeric(d1[idx1, column])
  hospitals <- d1[idx1,"Hospital.Name"]
  
  ##return the hospital
  idx2      <- order(rate,hospitals)
  hospitals <- hospitals[idx2]
  rate      <- rate[idx2]
  
  if( is.numeric(num) ) hospitals[num]
  else if (num == "best") hospitals[1]
  else hospitals[length(hospitals)]
}












