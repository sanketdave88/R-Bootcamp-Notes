rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  d <- read.csv("C:/Users/Sanket Dave/Desktop/Data Science/R/R-Bootcamp-Notes/Data/hopital-quality-data/outcome-of-care-measures.csv", colClasses = "character")
  ## Check that state and outcome are valid
  
  if( (state %in% d$State)!=TRUE ){
    stop('invalid state')
  } 
  
  if( (outcome %in% c("heart attack","heart failure","pneumonia") ) != TRUE ){
    stop('invalid outcome')
  }
  ## Return hospital name in that state with the given rank
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
  
  sorted.data.state <- data.state[order(as.numeric(data.state[[outcome]]),data.state[["Hospital.Name"]],decreasing=FALSE,na.last=NA), ]
  
  #handle num input
  if (num=="best") num = 1
  if (num=='worst') num = nrow(sorted.data.state)
  #will automatically return NA if num > nrow, as well as if it's some other text value
  # if someone passes num < 1, they'll get what's expected
  #if (is.numeric(num) & num > nrwo(sorted.data.state) return(NA)
  sorted.data.state[num,"Hospital.Name"]
}
