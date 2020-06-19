setwd("C:/Users/asus/Documents/RStudio Files/hospitaldataproject")

numhelp <- function(statesub, colnum,num) {
  ## get "attack", "failure" and "pneumonia" 
  outcomearray <- as.numeric(statesub[, colnum])
  len <- dim(statesub[!is.na(outcomearray), ])[1]
  
  if (num == "best") {
    rank <- rankhelp(statesub, outcomearray, 1)
  }
  else if(num == "worst") {
    rank <- rankhelp(statesub, outcomearray, len)
  }
  else if(num > len) {
    rank <- NA
  }
  else {
    rank <- rankhelp(statesub, outcomearray, num)
  }
  return(rank)
}

rankhelp <- function(statesub, outcomearray, num) {
  result <- statesub[, 2][order(outcomearray, statesub[, 2])][[num]]
  return(result)
}

rankall <- function(outcome, num = "best") {
  ## Read outcome data
  directory <- "C:/Users/asus/Documents/RStudio Files/hospitaldataproject/outcome-of-care-measures.csv"
  data <- read.csv(directory, colClasses = "character")
  
  ## Check that state and outcome are valid
  valid_outcomes <-  c("heart attack", "heart failure", "pneumonia")
  
  ## sorting state list and removing duplicates
  statearray <- sort(unique(data$State))
  
  ## number of states in total
  arraylength <- length(statearray)
  
  hospital <- rep("", arraylength)
  
  if(! outcome %in% valid_outcomes) {
    stop("invalid outcome")
  }
  
  else {
    for( i in 1:arraylength) {
      statesub <- data[data[, 7] == statearray[i], ]
      if(outcome == "heart attack") {
        hospital[i] <- numhelp(statesub, 11, num)
      }
      else if(outcome == "heart failure") {
        hospital[i] <- numhelp(statesub, 17, num)
      }
      else if(outcome == "pneumonia") {
        hospital[i] <- numhelp(statesub, 23, num)
      }
    }
  }
  ## For each state, find the hospital of the given rank
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  hospdataframe <- data.frame(hospital = hospital, statearray = statearray)
  return(hospdataframe)
}
