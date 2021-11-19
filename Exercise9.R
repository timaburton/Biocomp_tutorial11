# Exercise 09 

setwd("~/Desktop/Fall-2021/Biocomputing")
# Name of function and arguments

dir="~/Desktop/Fall-2021/Biocomputing/Biocomp_tutorial11/Example/"
column=1

# Takes directory as argument, specifies which column from each file to be used in calculations 
exercise9 <- function(dir, column, rows=50, override=FALSE){
# Reads data from each file in the specified directory
  file_list <- list.files(path=dir) # This function assumes that all of the files in the directory are .csv files 
  results<-NA
  for (i in 1:length(file_list)){
    data <- read.table(file_list[i])
    if(nrow(data)<rows){
      print("Error because number of rows in file does not meet the minimum requirement.")
      if (override==TRUE){
        # Save data and calculate coefficient of variation 
        stdev <- sd(data[,column])
        mean <- mean(data$column[i]==column)
        print("Coefficient of variation will be added to results even though file did not meet row requirement.")
        results <- c(results, (stdev/mean))
      }else{
        results <- c(results, NA) 
        }
    }else{
      # Calculate the standard deviation 
      stdev <- sd(data[,column])
      mean <- mean(data$column[i]==column)
      results <- c(results, (stdev/mean))
    } 
  }
  return(results[-1])
}


  




# Create directory with two csv files and have one column for each 