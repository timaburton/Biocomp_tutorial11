myfunction <- function(dir,sep=',',col=1,smallDataOverride=FALSE){
  #This function takes in four arguments:
  #   dir: directory to the file that is to be imported, in char
  #   sep: separation character
  #   col: specific column number to consider. Defaults to 1
  #   smallDataOverride: lets the function continue even with less than 50 points, defaults to FALSE
  
  
  #Perform input argument check
  #============================
  if(!is.character(dir) | !is.character(sep) | !is.logical(smallDataOverride) | !is.numeric(col)){
    print('One or more input values are of wrong data type')
    return()
  }

  
  #import data and take the relevant column. Check if the num of column is correct
  #============================
  raw <- read.table(file = dir, header = TRUE, sep = sep)
  if (ncol(raw) < col | col <= 0){
    print('ERROR: Invalid column number')
    return()
  }
  data <- raw[,col]

  
  #Check the data size, and override if given the option
  #============================
  if (length(data) <= 50 & !smallDataOverride){
    print('ERROR: Data size under 50. Override this with smallDataOverride argument')
    return()
  }else if (length(data) <= 50 & smallDataOverride){
    print('WARNING: The data set is small. Results might not be accurate')
  }
  
  
  #check if the data includes NA values or non numeric values
  #============================
  if(sum(is.na(data)) != 0){
    print('ERROR: Data includes NA values')
    return()
  }
  if(sum(!is.numeric(data)) != 0){
    print('ERROR: Data includes non numeric values')
    return()
  }
  
  
  #Calculate and return
  #============================
  return(sd(data) / mean(data))
}
