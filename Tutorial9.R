setwd("C:/Users/megan/Desktop/r-novice-inflammation/r-novice-inflammation-data/data")
dir <- list.files("../data", full.names = TRUE)
dir
fileCOV <- c()

coeffientOfVariation <- function(dir){
  csv <- dir[grep(".csv",dir)]
  table <- dir[grep(".txt",dir)]
  usable <- c(csv,table)
  for (i in 1:length(usable)){
    if (usable[grep(".csv", usable)] == TRUE){
    fil <- read.csv(file = usable[i], header = TRUE, stringsAsFactors = FALSE)
    }
    else {
      fil <- read.table(file = usable[i], sep = ,header = TRUE, stringsAsFactors = FALSE)
    }
    y <- sapply(fil,class)
    z <- colnames(fil)
    print(paste("Which column of the file would you like to calculate the coefficient of variation for? The columns are" , z , "/" ,y))
    print("Please only select a column of integers or numerics.")
    x <- as.name(readline())
    mean <- mean(fil[[x]])
    std <- sd(fil[[x]])
    cov <- std/mean
    fileCOV <- c(fileCOV, cov)
    i = i + 1
  }
  
}
file <- read.csv(file="wages.csv", header = TRUE, stringsAsFactors = FALSE)
y <- sapply(file,class)
z <- colnames(file)
print(paste("Which column of wages.csv would you like to calculate the coefficient of variation for? The columns are" , z , "/" ,y))
print("Please only select a column of integers or numerics.")
x <- as.name(readline())
mean <- mean(file[[x]])
std <- sd(file[[x]])
cov <- std/mean
fileCOV <- c(fileCOV, cov)
