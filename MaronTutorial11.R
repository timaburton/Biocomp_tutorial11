#Tutorial Assignment 12
setwd("/Users/erinmaron/Desktop/r-novice-inflammation/BioComp/ClassWork/Biocomp_tutorial11")

#Actual Function 
dir<-function(x,y){
  input<-list.files(x, pattern="*.csv",full.names = TRUE)
  data<-lapply(input,read.csv)
  stdev<-sd(data$y)
  mean<-mean(data$y)
  print((stdev/mean)*100)
  if (nrow(file)<50){
    print("warning: low level of data")
  }else if (nrow(file)>50){
    print("warning: entry does not have 50 entries")
  }
}
  

