complete  <- function(directory,id = 1:332){
  if(grepl('specdata',directory) == TRUE){
    directory <- "./Data/rprog_data_specdata/specdata"
  }
  
  complete_data <- numeric(length(id))
  
  allfiles <- as.character(list.files(directory))
  filepaths <- paste(directory,allfiles,sep = "/")
  
  j <- 1
  
  for(i in id){
    currentfile <- read.csv(filepaths[i], header = T, sep = ",")
    complete_data[j] <- sum(complete.cases(currentfile))
    j <- j + 1  
  }
  result <- data.frame(id = id,nobs = complete_data)
  result
}
