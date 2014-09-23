

## example code
outcome <- read.csv("data/outcome-of-care-measures.csv", colClasses = "character")
head(outcome)

outcome[, 11] <- as.numeric(outcome[, 11])
## You may get a warning about NAs being introduced; that is okay
hist(outcome[, 11])


## submit code
source("submitscript3.R")
submit()
