#Where x is a specified column in the files in dir 
path=("~/Desktop/Dir")
DirFiles=list.files(path=path, pattern="*.csv")
for (i in 1:DirFiles) {
  read.csv(i,header=TRUE,stringsAsFactors = FALSE, sep = ",")
}
varianceFunction<-function(dir,x){
  CoefVector<-vector()
  a=0
  b=0
  for (i in dir) {
  if(i[,x]<50){
    print("Warning! Less than 50 columns. If you wish to continue, type Yes")
    if (readline()=="Yes"){
      a<-mean(i[,x])
      b<-sd(i[,x])
      varianceCoef=b/a
      append(CoefVector, varianceCoef, after=length(CoefVector))
    }else{
      break
    }
  }else{
      a<-mean(i[,x])
      b<-sd(i[,x])
      varianceCoef=b/a
      append(CoefVector, varianceCoef, after=length(CoefVector))
      }
    }
  }
