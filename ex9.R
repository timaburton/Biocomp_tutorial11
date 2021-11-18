VC <- function(dir, column, observations = 0) {
  setwd(dir)
  list_files = list.files(path = dir)
  print(list_files)
  vars = NULL
  for (f in 1:length(list_files)) {
    data <- read.csv(file = list_files[f], header = T, sep = ",", stringsAsFactors = FALSE)
    obs = nrow(data)
    if (observations == 0){
      if (obs < 50) {
        print("Not enough observations")
        next
      }
      else {
        sd_col = sd(data[, column])
        mean_col = mean(data[, column])
      }
    }
    else if (observations == 1) {
      if (obs < 50) {
        print("< 50 observations.")
        sd_col = sd(data[, column])
        mean_col = mean(data[, column])
      }
      else {
        sd_col = sd(data[, column])
        mean_col = mean(data[, column])
      }
    }
    variationCoeff = sd_col / mean_col
    variations = c(variations, variationCoeff)
  }
  return(vars)
}

