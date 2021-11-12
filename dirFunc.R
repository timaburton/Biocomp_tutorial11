# function call: array = dir(dirpath = full_path_to_directory, col = int, obs = bool)

rm(list=ls())

dir<-function(dirpath=".", col, obs=T){
  # get a list of the files in the directory
  files <- list.files(dirpath, pattern=".csv")
  
  # Check input validity
  # check that col is an integer
  if(col != round(col)) {
    print("Invalid col Value")
    return ()
  }
  # check directory path
    # user must input full path to directory
  if(!file.exists(dirpath)) {
    print("Invalid dirpath Value")
    return ()
  }
  
  # initialize variables
  setwd(file.path(dirpath))
  result <- c()
  
  # Loop through each file and calculate the coef dev
  for (i in seq_along(files)) {
    filetemp = read.csv(files[i], header=T, stringsAsFactors =F)
    # check that col is valid
    if (ncol(filetemp) < col) {
      cat("Error with file", files[i], ": invalid col\n")
      return()
    }
    # check if there are 50 observations
    if (nrow(filetemp) < 50) {
      if (obs){
        # print error with number of observations and return null
        cat("Error with file", files[i], ": not enough observations\n")
        return ()
      } else {
        # if user overrides, just print a warning
        cat("Warning: file", files[i], "has less that 50 observations\n")
      }
    }
    # calculate coefdev and append to result
    coefDev = sd(filetemp[,col], na.rm=T)/mean(filetemp[,col], na.rm=T)
    result = c(result, coefDev)
  }
  
  # return result if eveything is successful
  return (result)
}

print(dir(col=1))
