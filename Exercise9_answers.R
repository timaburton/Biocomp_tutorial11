
variancefunction <- function(dir,x){
  files <- list.files(path = dir, pattern = "*.csv")

finalvector <- vector()
for(i in files){
  a = 0
  b = 0
  if(i[,x] < 50){
    print("Error less than 50 entries, type yes to continue, or anything else to break")
    if(readline() == "yes"){
      a<-mean(i[,x])
      b<-sd(i[,x])
      variancecoef = b/a
      append(finalvector,variancecoef, after = length(finalvector))
    }else{
      break
    }
  }else{
    a<-mean(i[,x])
    b<-sd(i[,x])
    variancecoef = b/a
    append(finalvector,variancecoef, after = length(finalvector))
  }
}

}
