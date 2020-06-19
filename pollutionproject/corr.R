setwd("C:/Users/asus/Documents/RStudio Files/specdata")

corr <- function(directory, threshold = 0) {
  if(grep("specdata", directory) == 1) {
    directory <- ("./specdata/")
  }
  complete_table <- complete("specdata", id = 1:332)
  nobs <- complete_table$nobs
  
  #finding a valid id
  ids <- complete_table$id[nobs > threshold]
  
  id_len <- length(ids)
  corr_vector <- rep(0, id_len)
  
  all_files <- list.files(directory)
  file_paths <- paste(directory, all_files, sep = "")
  
  j <- 1
  
  for(i in ids) {
    current_file <- read.csv(file_paths[i], sep = ",")
    corr_vector[j] <- cor(current_file$sulfate, current_file$nitrate, use = "complete.obs")
    j <- j + 1
  }
  result <- corr_vector
  return(result)
}