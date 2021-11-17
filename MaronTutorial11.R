#Tutorial Assignment 12
setwd("/Users/erinmaron/Desktop/r-novice-inflammation/BioComp/ClassWork/Biocomp_tutorial11")
dir<-function(directory, x){
  data<-read.table("*.csv")
  stdev<-stdev(data$x)
  output(for(i in stdev))
  if (nrow(file)<50){
    print("warning: low level of data")
  } else if (nrow(file)>50){
    print("warning: entry does not have 50 entries")
  }
}