setwd("C:/Users/megan/Desktop/r-novice-inflammation/r-novice-inflammation-data/data")
dir <- list.files("../data", full.names = TRUE)
dir
fileCOV <- c()
usable <- c()
coeffientOfVariation <- function(dir){
  csv <- dir[grep(".csv",dir)]
  table <- dir[grep(".txt",dir)]
  usable <- c(csv,table)
  for (i in 1:length(usable)){
    fil <- read.csv(file = usable[i], header = TRUE, stringsAsFactors = FALSE)
    y <- sapply(fil,class)
    z <- colnames(fil)
    print(paste("Which column of the file would you like to calculate the coefficient of variation for? The columns are" , z , "/" ,y))
    print("Please only select a column of integers or numerics.")
    x <- as.name(readline())
    if (length(fil[[x]]) < 50){
      print("An accurate coefficient of variation should have at least 50 entries, and this column has less than 50.")
      question <- (readline("Would you like to continue? Yes or No"))
      if (question == "YES"){
        mean <- mean(fil[[x]])
        std <- sd(fil[[x]])
        cov <- std/mean
        fileCOV <- c(fileCOV, cov)
        i = i + 1
      }else {
        i = i +1
      }
    }
    else {
    mean <- mean(fil[[x]])
    std <- sd(fil[[x]])
    cov <- std/mean
    fileCOV <- c(fileCOV, cov)
    i = i + 1
    }
  }
  
}
coeffientOfVariation(dir)



file <- read.csv(file="car-speeds.csv", header = TRUE, stringsAsFactors = FALSE)
y <- sapply(file,class)
z <- colnames(file)
print(paste("Which column of wages.csv would you like to calculate the coefficient of variation for? The columns are" , z , "/" ,y))
print("Please only select a column of integers or numerics.")
x <- as.name(readline())
if (length(file[[x]]) < 50){
  print("An accurate coefficient of variation should have at least 50 entries, and this column has less than 50.")
  question <- (readline("Would you like to continue? Yes or No"))
  if (question == "YES"){
    mean <- mean(file[[x]])
    std <- sd(file[[x]])
    cov <- std/mean
    fileCOV <- c(fileCOV, cov)
  }else{
    print("Okay")
  }
}else {
  mean <- mean(file[[x]])
  std <- sd(file[[x]])
  cov <- std/mean
  fileCOV <- c(fileCOV, cov)
}


length(file$wage)

mean <- mean(file[[x]])
std <- sd(file[[x]])
cov <- std/mean
fileCOV <- c(fileCOV, cov)

if (question == "YES"){
  mean <- mean(file[[x]])
  std <- sd(file[[x]])
  cov <- std/mean
  fileCOV <- c(fileCOV, cov)
}else{
  print("Okay")
}