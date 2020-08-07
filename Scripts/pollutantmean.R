pollutantmean <- function(directory, pollutant, id = 1:332){
    
    #set working directory
    if(grepl("specdata",directory) == TRUE){
      directory <- "./Data/rprog_data_specdata/specdata"
    }
  
    #vector to hold pollutant data
    mean_vector <- c()
    
    #find all files in specdata folder
    allfiles <- as.character(list.files(directory))
    filepaths <- paste(directory,allfiles,sep = "/")   
    
    for (i in id){
      currentfile <- read.csv(filepaths[i],header = TRUE,sep = ",",)
      na_removed <- currentfile[!is.na(currentfile[, pollutant]), pollutant]
      mean_vector <- c(mean_vector,na_removed)
    }
    
    result <- mean(mean_vector)
    result
}
