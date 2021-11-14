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

#Need to figure out how to override a default for a warning not an error. By having two defaults, changing them gives the output of 50+, one 
#gives an error for being under 50 with req="Yes", and the last gives a Warning for req="No".By changing x, the minimum number of observations
#is changed. 

VarCof<-function(dir, column, x=50, req="Yes"){
  #List files in a variable
  file.list<-list.files(dir)
  #Empty vector to add results to
  vector<-c()
  #For each of the files, pull out the column of interest
  for (i in 1:length(file.list)){
    #Read the file as a table so the column can be pulled out
    file<-read.table(file.list[i], header = TRUE, stringsAsFactors = FALSE)
    #Pull out the column of interest
    interestcol<-file[, column]
    #Find the number of observations
    if(length(interestcol)>x){ #More than X, no error message
      #Calculate, mean, standard deviation, and variation
      M<-mean(interestcol)
      SD<-sd(interestcol, na.rm = FALSE)
      Var<-SD/M
      vector<-c(vector, Var)
      }else if (length(interestcol)<x && req = "Yes"){
      print("Error: There are not enough observations to calculate a reliable coefficient of variation")
        vector<-c(vector, NA)
      }else if (length(interestcol)<x && req = "No"){
        print("Warning: There are not enough observations to calculate a reliable coefficient of variation")
        M<-Mean(interestcol)
        SD<-sd(interestcol)
        Var<-SD/M
        vector<-c(vector, Var)
      }
     }
    return(vector)
  }



