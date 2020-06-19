setwd("C:/Users/asus/Documents/RStudio Files/hospitaldataproject")
list.files()

## function to find hospital name
hname <- function(data, colnum, state){
  statesub <- data[data[, 7] == state, ]
  outcomearray <- statesub[, colnum]
  min <- min(outcomearray, na.rm = TRUE)
  minindex <- which(outcomearray == min)
  hospname <- statesub[minindex, 2]
  return(hospname)
}

best <- function(state, outcome) {
  ## Read outcome data
  directory <- "C:/Users/asus/Documents/RStudio Files/hospitaldataproject/outcome-of-care-measures.csv"
  data <- read.csv(directory, colClasses = "character")
  
  ## Check that state and outcome are valid
  ## Converting into a numeric vector
  data[, 11] <- as.numeric(data[, 11]) # heart attack
  data[, 17] <- as.numeric(data[, 17]) # heart failure
  data[, 23] <- as.numeric(data[, 23]) # pneumonia
  valid_outcomes <- c("heart attack", "heart failure", "pneumonia")
  
  if(! state %in% data$State){
    stop("invalid state")
  }
  
  else if(! outcome %in% valid_outcomes){
    stop("invalid outcome")
  }
  
  else {
    if(outcome == "heart attack") {
      hosp_name <- hname(data, 11, state)
    }
    else if(outcome == "heart failure") {
      hosp_name <- hname(data, 17, state)
    }
    else if(outcome == "pneumonia") {
      hosp_name <- hname(data, 23, state)
    }
    
  }
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  return(hosp_name)
}
