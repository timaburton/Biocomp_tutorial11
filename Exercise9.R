# Exercise 9
# here it is assumed that all the files are CSV files (although the user can specify the delimiter type)
# works only if the directory contains files that have the same separator



VarCoefficient <- function(dir, column, observations = 0, delimiter = ",") {
  setwd(dir)
  list_files = list.files(path = dir)
  variations = NULL
  for (f in 1:length(list_files)) {
    data <- read.csv(file = list_files[f], header = T, sep = delimiter, stringsAsFactors = FALSE)
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
    sd_col = sd(data[, column])
    mean_col = mean(data[, column])
    variationCoeff = sd_col / mean_col
    variations = c(variations, variationCoeff)
  }
  return(variations)
}

VarCoefficient("/Users/johi/Desktop/Courses/Biocomputing/r-novice-inflammation/infl_data", 5, observations = 0)

