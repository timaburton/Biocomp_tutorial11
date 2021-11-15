### Biocomputing Exercise09 - Pulling the R pieces together

# Create the coefficient of variation vector
coefficientofvariationVector = vector(mode="numeric", 0)

# Set a working directory 
setwd(dir)
inputfiles<-list.files(dir)

# Sets up a loop for the files in the directory to calculate the coefficient of variation
for (file in inputfiles){
  file<- read.table(file, header=TRUE, sep=",", stringsAsFactors = FALSE)
}