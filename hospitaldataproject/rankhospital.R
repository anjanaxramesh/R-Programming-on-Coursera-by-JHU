setwd("C:/Users/asus/Documents/RStudio Files/hospitaldataproject")

numhelp <- function(data, colnum, state, num) {
  statesub <- data[data[, 7] == state, ]
  ## get "attack", "failure" and "pneumonia" 
  outcomearray <- statesub[, colnum]
  len <- dim(statesub[!is.na(outcomearray), ])[1]
  if(num == "worst") {
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

rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  directory <- "C:/Users/asus/Documents/RStudio Files/hospitaldataproject/outcome-of-care-measures.csv"
  data <- read.csv(directory, colClasses = "character")
  
  ## Check that state and outcome are valid
  ## Converting into a numeric vector
  data[, 11] <- as.numeric(data[, 11]) # heart attack
  data[, 17] <- as.numeric(data[, 17]) # heart failure
  data[, 23] <- as.numeric(data[, 23]) # pneumonia
  valid_outcomes <- c("heart attack", "heart failure", "pneumonia")
  
  ## Check that state and outcome are valid
  if(! state %in% data$State){
    stop("invalid state")
  }
  
  else if(! outcome %in% valid_outcomes){
    stop("invalid outcome")
  }
  
  else {
    if(num == "best") {
      rank <- best(state, outcome)
    }
    else {
      if(outcome == "heart attack") {
        rank <- numhelp(data, 11, state, num)
      }
      else if(outcome == "heart failure") {
        rank <- numhelp(data, 17, state, num)
      }
      else if(outcome == "pneumonia") {
        rank <- numhelp(data, 23, state, num)
      }
    }
  }
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  return(rank)
}