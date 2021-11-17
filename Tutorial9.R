setwd("C:/Users/megan/Desktop/r-novice-inflammation/r-novice-inflammation-data/data")
dir <- list.files("../data", full.names = TRUE)

fileCOV <- c()
coeffientOfVariation <- function(dir){
  
  
}
wage <- read.csv(file="wages.csv", header = TRUE, stringsAsFactors = FALSE)
y <- sapply(wage,class)
z <- colnames(wage)
print(paste("Which column of wages.csv would you like to calculate the coefficient of variation for? The columns are" , z , "/" ,y))
print("Please only select a column of integers or numerics.")
x <- as.name(readline())
mean <- mean(wage[[x]])
std <- sd(wage[[x]])
cov <- std/mean
fileCOV <- c(fileCOV, cov)
