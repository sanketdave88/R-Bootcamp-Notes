best <- function(state, outcome) {
  ## Read outcome data
  d <- read.csv("C:/Users/Sanket Dave/Desktop/Data Science/R/R-Bootcamp-Notes/Data/hopital-quality-data/outcome-of-care-measures.csv", colClasses = "character")
  ## Check that state and outcome are valid
  
    if( (state %in% d$State)!=TRUE ){
      stop('invalid state')
    } 
 
    if( (outcome %in% c("heart attack","heart failure","pneumonia") ) != TRUE ){
      stop('invalid outcome')
    }
  
  ## Return hospital name in that state with lowest 30-day death
  if (outcome == "heart attack"){
    outcome <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
  }
  else if(outcome == "heart failure"){
    outcome <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
  }
  else if(outcome == "pneumonia"){
    outcome <- "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
  }
  
  data.state <- d[d$State == state,]
  idx <- which.min(as.double(data.state[,outcome]))
  data.state[idx,"Hospital.Name"]
  ## rate
}
