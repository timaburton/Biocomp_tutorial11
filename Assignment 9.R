#Exercise 9
  
#Set working directory, create vector for coefficient of variation
setwd(dir)
files<-list.files(dir)
CoefVar<-function(dir, col, nrow=50){
CoefVarVector<-numeric(0)
  
#Loop files in directory to determine coefficient of variation
for (i in files){
  i<-read.csv(i, sep=",", header=TRUE, stringsAsFactors=FALSE, fill=TRUE)
  if(ncol(i)<col){
    print("ERROR invalid column number")
  }else{
    if(nrow(i)<50){
      Q<-print("ERROR invalid number of observations. Input O to override or any other key to stop: ")
      if(Q=="O"){
        standdev=sd(i[,col])
        mean=mean(i[,col])
        CoefVar=standdev/mean
        CoefVarVector=c(CoefVarVector, CoefVar)
      }else{
        print("CYA!")
      }
    }else{
      standdev=sd(i[,col])
      mean=mean(i[,col])
      CoefVar=standdev/mean
      CoefVarVector=c(CoefVarVector, CoefVar)
    }
  }
}
return(CoefVarVector)
}