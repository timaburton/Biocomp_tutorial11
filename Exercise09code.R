# Usage: (directory_path, specified_column)
coeffOfVar <- function(dir,colName){
  fileList <- list.files(path = dir)
  vectorOutput <- c()
  for(i in fileList){
    data <- read.table(i,header=TRUE,sep=",",stringsAsFactors=FALSE)
    rows <- length(data$colName)
    minRow <- 50
    if(rows<minRow){
      answer <- readline(prompt = "WARNING: input files have less than 50 observations. 
                         Would you still like to calculate the coefficient of variance? 
                         Type 'Y' for yes or 'N' for no")
      if(answer=="Y"){
        Sd=sd(data$colName)
        Mean=mean(as.numeric(data$colName))
        cov=Sd/Mean
        vector <- c(vectorOutput, cov)
      }
      if(answer=="N"){
        break
      }
    }else{
      Sd=sd(data$colName)
      Mean=mean(as.numeric(data$colName))
      cov=Sd/Mean
      vector <- c(vectorOutput, cov)
    }
  }
  return(vectorOutput)
}

#Test code:
coeffOfVar("~/Desktop/Biocomputing/RStudio/Biocomp_tutorial11/new",wage)