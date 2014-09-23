best <- function(state, outcome) 
{
  ## Read outcome data
  data <- read.csv("data/outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that state and outcome are valid
  if(! outcome %in% c("heart attack", "heart failure", "pneumonia"))
    stop("invalid outcome")
  
  if(! state %in% data$State)
    stop("invalid state")
  
  ## Return hospital name in that state with lowest 30-day death rate
  column <- 
    if( outcome == "heart attack") { 11 }
    else if( outcome == "heart failure") { 17 }
    else { 23 }
  
  d1   <- data[data$State == state,]
  ## clean non numeric data
  idx1 <- d1[, column] != "Not Available"
  min  <- min(as.numeric(d1[idx1, column]))
  
  ## indeces of hospitals with lowest value
  d2        <- d1[idx1,]
  idx2      <- which(as.numeric(d2[, column]) == min)
  hospitals <- d2[idx2,"Hospital.Name"]
  
  ##return the first hopital in alphabetical order
  sort(hospitals)[1]
}



