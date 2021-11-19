# Exercise 09 

setwd("~/Desktop/Fall-2021/Biocomputing/Biocomp_tutorial11")
# Name of function and arguments

# Takes a directory path and column number, default arguments are set for rows and override 
exercise9 <- function(dir, column, rows=50, override=FALSE){
# Reads data from each file in the specified directory
  file_list <- list.files(path=dir) # This function assumes that all of the files in the directory are .csv files 
  results<-NA # Creates results vector 
  for(i in 1:length(file_list)){ 
    data <- read.table(file_list[i])
    if(nrow(data)<rows){ # Tests whether the number of rows in each file meets the specified row requirement
      print(paste0("Error because number of rows in ", file_list[i], " does not meet the minimum requirement."))
      if (override==TRUE){ # If override is true, then the coefficient of standard deviation will still be calculated 
        print(paste0("Coefficient of variation for ", file_list[i], " will be added to results vector even though file did not meet row requirement."))
        stdev <- sd(data[,column])
        mean <- mean(data[,column])
        results <- c(results, (stdev/mean))
      }else{ # If override is false and rows do not meet the requirement, NA will be added to the results vector 
        results <- c(results, NA) 
        }
    }else{
      # If rows are greater than 50, add the coefficient of standard deviation to the results vector for each file 
      stdev <- sd(data[,column])
      mean <- mean(data[,column])
      results <- c(results, (stdev/mean))
    } 
  }
  return(results[-1])
}

# Call function 
## Give path to directory for dir argument
## Give number of column for column argument 
## Optional to change default number of rows and override
exercise9(dir,column)
  
