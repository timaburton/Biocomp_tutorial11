#Tutorial Assignment 12
setwd("/Users/erinmaron/Desktop/r-novice-inflammation/BioComp/ClassWork/Biocomp_tutorial11")

#Calling Directory
dir<-function(directory,column){
  files<-list.files(pattern="*.csv") #pulls list of files from directory
  for(i in 1:length(files)){
    data<-read.table(files[i]) #read files within directory 
    stdev<-sd(data$column==column, na.rm=FALSE) #find and set the standard deviation of data
    mean<-mean(data$column==column) #repeat with the mean 
    }
  if(nrow(files>=50)){
      return((stdev/mean)*100) #if the data has 50 or more points, print an error 
    }else{
      print("Warning:Less than 50 data point", cdev) #if there is insufficient data, print an error but continue to prvide output
    }
}

#Keep getting error with calling a column name in arguement, but cannot seem to find what to fix after looking at notes and googling, so this is as far as I could get
