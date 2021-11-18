# Exercise 9

# Here it is assumed that all the files are CSV files (although the user can specify the delimiter type)
# Works only if the directory contains files that have the same separator

# USAGE: VarCoefficient(dir, column, observations = 0, delimiter = ",")
# dir is the user specified directory
# column is the user specified column in each file in the directory
# observations (default is 0) specifies if the function prints out an error when encountering file with less than 50 observations (user can set the observations argument equal to 1, in that case the error message is overrun and only warning message is given)
# delimiter allows the user to specify the file separator type (default is comma separated, but user could also specify tab separated).


VarCoefficient <- function(dir, column, observations = 0, delimiter = ",") {
  setwd(dir)
  list_files = list.files(path = dir)
  variations = NULL
  for (f in 1:length(list_files)) {
    data <- read.table(file = list_files[f], header = T, sep = delimiter, stringsAsFactors = FALSE)
    obs = nrow(data)
    if (observations == 0){
      if (obs < 50) {
        print("Error! Too few observations!")
        next
      }
      else {
      }
    }
    else if (observations == 1) {
      if (obs < 50) {
        print("Warning! Less than 50 observations.")
      }
      else {
      }
    }
    variationCoeff = sd(data[, column]) / mean(data[, column])
    variations = c(variations, variationCoeff)
  }
  return(variations)
}

VarCoefficient("/Users/johi/Desktop/Courses/Biocomputing/r-novice-inflammation/infl_data", 5, observations = 1)

