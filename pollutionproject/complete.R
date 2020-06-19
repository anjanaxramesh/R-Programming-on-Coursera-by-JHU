setwd("C:/Users/asus/Documents/RStudio Files/specdata")

complete <- function(directory, id = 1:332) {
  if(grep("specdata", directory) == 1) {
    directory <- ("./specdata/")
  }
  id_len <- length(id)
  complete_data <- rep(0, id_len)
  
  #find all files in the specdata folder
  all_files <- list.files(directory)
  file_paths <- paste(directory, all_files, sep = "")
  
  j <- 1
  
  for(i in id) {
    current_file <- read.csv(file_paths[i], sep = ",")
    complete_data[j] <- sum(complete.cases(current_file))
    j <- j + 1
  }
  result = data.frame(id = id, nobs = complete_data)
  return(result)
}
