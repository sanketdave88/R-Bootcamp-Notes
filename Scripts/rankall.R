rankall <- function(outcome, num = "best") {
  ## Read outcome data
  d <- read.csv("C:/Users/Sanket Dave/Desktop/Data Science/R/R-Bootcamp-Notes/Data/hopital-quality-data/outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that state and outcome are valid
  validState = sort(unique(d[,7]))
  
  if( (outcome %in% c("heart attack","heart failure","pneumonia") ) != TRUE ){
    stop('invalid outcome')
  }
  
  ##getting original column name
  if (outcome == "heart attack"){
    outcome <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
  }
  else if(outcome == "heart failure"){
    outcome <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
  }
  else if(outcome == "pneumonia"){
    outcome <- "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
  }
  
  hospital <- character(0)
  ## For each state, find the hospital of the given rank
  for (i in seq_along(validState)) {
    ## Return hospital name in that state with the given rank 30-day death rate
    data.state <- d[d$State==validState[i],]
    
    # order data by outcome
    sorted.data.state <- data.state[order(as.numeric(data.state[[outcome]]),data.state[["Hospital.Name"]],decreasing=FALSE,na.last=NA), ]
    
    #handle num input
    this.num = num
    if (this.num=="best") this.num = 1
    if (this.num=='worst') this.num = nrow(sorted.data.state)
    
    hospital[i] <- sorted.data.state[this.num,"Hospital.Name"]
  }
  
  ## Return a data frame with the hospital names and the (abbreviated) state name
  data.frame(hospital=hospital,state=validState,row.names=validState)
}

