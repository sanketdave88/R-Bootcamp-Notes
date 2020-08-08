corr <- function(directory, threshold = 0){
  if(grepl("specdata",directory) == TRUE){
    directory <- "./Data/rprog_data_specdata/specdata"
  }
  # get the complete table
  complete_table <- complete("specdata",1:332)
  nobs <- complete_table$nobs
  
  # find valid ids
  ids <- complete_table$id[nobs > threshold]
  
  # length of ids vector
  id_len <- length(ids)
  corr_vec <- numeric(id_len)
  
  # find all the files in specdata folder
  allfiles <- as.character(list.files(directory))
  filepaths <- paste(directory,allfiles,sep = "/")
  j <- 1
  for(i in ids){
    currentfile <- read.csv(filepaths[i],header = T,sep = ",")
    corr_vec[j] <- cor(currentfile$sulfate,currentfile$nitrate,use = "complete.obs")
    j <- j + 1
  }
  result <- corr_vec
  result
}
