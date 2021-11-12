coeff_of_var <- function(dir,col,override="off"){
  
  # Set the working directory to specified directory
  
  setwd(dir)
  
  # Create an empty vector that will be filled with the coefficient of variations 
  # calculated for each file in the specified working directory
  
  coeff_of_var <- numeric(0) 

  for (filename in list.files()){
    # Read file
    file <- read.table(filename, header = FALSE, sep = ",", stringsAsFactors = FALSE)
    # Determine if the indicated column input is valid
    if (col <= ncol(file)){
      # Determine if contents of file meet (or do not meet) the recommended 50 observations 
      # for the calculation of the coefficient of variations
      if (length(file[,col]) >= 50){
        # Calculate coefficient of Variation
        mean <- mean(file[,col])
        stdev <- sd(file[,col])
        coefficient_of_variation <- stdev/mean
        # Add each calculated coefficient of variation to a vector
        coeff_of_var <- c(coeff_of_var,coefficient_of_variation)
      } 
      else if (length(file[,col]) < 50 && override=="off"){
        print(paste0("Error: ", filename, " should have at least 50 observations in order to calculate a reliable coefficient of variation."))
        coeff_of_var <- c(coeff_of_var,NA)
      } 
      else if (length(file[,col]) < 50 && override=="on"){
        print(paste0("Warning: It is recommended ", filename, " have at least 50 observations in order to calculate a reliable coefficient of variation."))
        # Calculate coefficient of variation
        mean <- mean(file[,col])
        stdev <- sd(file[,col])
        coefficient_of_variation <- stdev/mean
        # Add each calculated coefficient of variation to a vector
        coeff_of_var <- c(coeff_of_var,coefficient_of_variation)
      } 
      else {
        print("Please enter valid arguments.")
        break
      }
    } 
    else {
      print(paste0("Warning: ", filename, " was skipped because it did not contain the specified column number."))
      next
    }
  
  }
  return(coeff_of_var)
}