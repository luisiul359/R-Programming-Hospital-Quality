##returns a 2-column data frame containing the hospital in each state that has the ranking specified in num. 

rankall <- function(outcome, num = "best") 
{
  ## Read outcome data
  data <- read.csv("data/outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that state and outcome are valid
  if(! outcome %in% c("heart attack", "heart failure", "pneumonia"))
    stop("invalid outcome")
   
  column <- 
    if( outcome == "heart attack") { 11 }
    else if( outcome == "heart failure") { 17 }
    else { 23 }
  
  hospital <- character(0)
  state    <- character(0)
  
  ## For each state, find the hospital of the given rank
  for( s in unique(data$State))
  {
    d1   <- data[data$State == s,]
    ## check if num is larger than the number of hospitals in that state
    if( is.numeric(num) && nrow(d1) < num)
    {
      hospital <- c(hospital, NA)
      state <- c(state, s)
      next
    }
    
    ## clean non numeric data
    idx1 <- d1[, column] != "Not Available"
    ## get rate and hospitals
    rate      <- as.numeric(d1[idx1, column])
    hospitals <- d1[idx1,"Hospital.Name"]
    
    ## reorder
    idx2      <- order(rate, hospitals)
    hospitals <- hospitals[idx2]
    rate      <- rate[idx2]
    
    h <-
      if( is.numeric(num) ) hospitals[num]
      else if (num == "best") hospitals[1]
      else hospitals[length(hospitals)]
    
    hospital <- c(hospital, h)
    state    <- c(state, s)
  }
  
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  idx      <- order(state, hospital)
  hospital <- hospital[idx]
  state    <- state[idx]
  
  data.frame(hospital = hospital, state = state)
}



