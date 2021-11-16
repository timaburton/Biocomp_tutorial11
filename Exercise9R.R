#Marlee Shaffer
##Exercise 9 

#VarCof is a function that reads data from each file in the specified directory 
#and calculates the coefficient of variation. By default, 50 observations are required
#to get an output.

#Usage: VarCof(dir = "full directory map", column = "number of column of interest", 
#x = minimum number of observations, req = is override on or off (off by default))

VarCof<-function(dir, column, x=50, req="Yes"){
  #Set the working directory to the directory entered 
  setwd(dir)
  #List files in a variable
  file.list<-list.files(dir)
  #Empty vector to add results to
  vector<-c()
  #For each of the files, pull out the column of interest
  for (files in file.list){
    #Read the file as a table so the column can be pulled out
    file<-read.table(files, header = TRUE, sep=",", stringsAsFactors = FALSE)
    #Pull out the column of interest
    interestcol<-file[, column]
    #Find the number of observations
    if(length(interestcol)>=x){ #More than or equal to X, no error message
      #Calculate, mean, standard deviation, and variation
      M<-mean(interestcol)
      SD<-sd(interestcol, na.rm = FALSE)
      Var<-SD/M
      vector<-c(vector, Var)
      }else if (length(interestcol)<x && req == "Yes"){ #Less than X, no override, gives warning 
      print("Error: There are not enough observations to calculate a reliable coefficient of variation")
        #vector<-c(vector, NA)
      }else if (length(interestcol)<x && req == "No"){ #Less than X, override. gives warning but solves
        print("Warning: There are not enough observations to calculate a reliable coefficient of variation")
        M<-Mean(interestcol)
        SD<-sd(interestcol)
        Var<-SD/M
        vector<-c(vector, Var)
      }else{
        print("Check arguement inputs")
      }
     }
    return(vector)
  }



