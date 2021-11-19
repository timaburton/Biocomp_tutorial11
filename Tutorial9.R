setwd("C:/Users/megan/Desktop/r-novice-inflammation/r-novice-inflammation-data/data")
#list of files
dir <- list.files("../data", full.names = TRUE)
dir
#return vector
fileCOV <- c()
#only csv files vector
usable <- c()
coeffientOfVariation <- function(dir){
  #only csv files
  usable <- dir[grep(".csv", dir)]
  #for number of csv files
  for (i in 1:length(usable)){
    #read file
    fil <- read.csv(file = usable[i], header = TRUE, stringsAsFactors = FALSE)
    #column type
    y <- sapply(fil,class)
    #column names
    z <- colnames(fil)
    print(paste("Which column of the file would you like to calculate the coefficient of variation for? The columns are" , z , "/" ,y))
    print("Please only select a column of integers or numerics.")
    #input from user
    x <- as.name(readline())
    #test if the column has more than 50 entries
    if (length(fil[[x]]) < 50){
      print("An accurate coefficient of variation should have at least 50 entries, and this column has less than 50.")
      question <- (readline("Would you like to continue? Yes or No: "))
      #if less than 50 entries ask user if they want to continue
      if (question == "Yes"){
        #mean and removes NA occurrences
        mean <- mean(fil[[x]], na.rm = TRUE)
        #standard deviation and removes NA occurrences
        std <- sd(fil[[x]], na.rm = TRUE)
        #coefficient of variation
        cov <- std/mean
        #return vector
        fileCOV <- c(fileCOV, cov)
        i = i + 1
      }else {
        i = i +1
      }
    }
    #if over 50 entries
    else {
      #mean and removes NA occurrences
    mean <- mean(fil[[x]], na.rm = TRUE)
    #standard deviation and removes NA occurrences
    std <- sd(fil[[x]], na.rm = TRUE)
    #coefficient of variation
    cov <- std/mean
    #return vector
    fileCOV <- c(fileCOV, cov)
    i = i + 1
    }
  }
  #actual returning of vector
  return(fileCOV)
}
coeffientOfVariation(dir)
