### Biocomputing Exercise09 - Pulling the R pieces together

# Defining the function
CoefficientofVariation <- function(dir,col,nrow=50){
 
   # Set a working directory and create the Coefficient of Variation vector
  setwd(dir)
  inputfiles<-list.files(dir)
  coefficientofvariationVector <- numeric(0)
  
  # Sets up a loop for the files in the directory to calculate the coefficient of variation
  for (i in inputfiles){
    i <- read.csv(i,header=TRUE,sep=",",stringsAsFactors=FALSE, fill=TRUE)
    # Checking if the file has the correct number of columns
    if(ncol(i)< col){
      print("Invalid column number.")
    }else{
      # Checking if the column has 50 observations
      if(nrow(i) <50){
        # Offers the override option if the column does not have 50 observations
        not50 <- print("Recommended that the file has at least 50 observations to calculate a coefficient of variation. Would you like to proceed? Type 'Y' for Yes and 'N' for No: ")
        if(not50=="Y"){
          mean=mean(i[,col])
          standarddeviation=sd(i[,col])
          coefficientofvariation=standarddeviation/mean
          coefficientofvariationVector=c(coefficientofvariationVector,coefficientofvariation)
        }else{
          # Output if the user chooses not to override
          print("Function stopped.")
        }
      }else{
        mean=mean(i[,col])
        standardeviation=sd(i[,col])
        coefficientofvariation=standardeviation/mean
        coefficientofvariationVector=c(coefficientofvariationVector,coefficientofvariation)
      }
    }
  }
  
  return(coefficientofvariationVector)
}
  



