#Marlee Shaffer
##Exercise 9 

#Write a function that takes a directory name as an argument called dir plus any other arguments required to
#accomplish the specified task.

#The function should read data from each file in the specified directory and calculate the coefficient of variation
#(standard deviation divided by the mean) for a user specified column. These values should be returned as a
#vector.

#To calculate a reliable coefficient of variation we would like to have 50 observations, but we also don’t want
#to force the user to use our high standard for the data. Make your function, by default, report an error if any
#file has less than 50 observations, but allow the user to override this behavior and only receive a warning if 50
#observations are not present in a file.

#For an extra credit point, add arguments and associated code to your function to situations where a file
#doesn’t have the correct number of columns or the provided data includes NA’s.

VarCof<-function(dir, column, x=50){
  #List files in a variable
  file.list<-list.files(dir)
  vector<-c()
  #For each of the files, pull out the column of interest
  for (i in 1:length(file.list)){
    interestcol<-file.list[, file.list$column]
    if{(length(interestcol)>x)
      M<-mean(interestcol)
      SD<-sd(interestcol, na.rm = FALSE)
      output<-SD/M
    }else if {(length(interestcol)<x)
      print("There are not enough observations")
      M<-mean(interestcol)
      SD<-sd(interestcol, na.rm = FALSE)
      output<-SD/M
    }
  }
}
