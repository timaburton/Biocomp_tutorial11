#VarCof is a function that reads data from each file in the specified directory 
#and calculates the coefficient of variation. By default, 50 observations are required
#to get an output. The column of interest must be numerical.
VarCof<-function(dir, col, x=50, override="No"){
  #Set the working directory to the directory entered
  setwd(dir)
  #List all files in the directory and save them as a variable
  File<-list.files(dir)
  #Create an empty vector for variance
  Variance<-c()
  #Create an empty vector for error
  Error<-c()
  #For each file..
  for (files in File){
    #Read the file as a table so the columns can be pulled out 
    file_i<-read.table(files, header = TRUE, sep=",", stringsAsFactors = FALSE)
    #Pull out column of interest
    interestcol<-file_i[, col]
    interestcol.na<-na.omit(interestcol)
    #Find the number of observations and do math depending on value of x and override
    if (length(interestcol.na)>=x){ #Greater than or equal to 50, or changed value
      #Calculate the mean, standard deviation, and variation
      M<-mean(interestcol.na)
      SD<-sd(interestcol.na, na.rm = FALSE)
      Var<-SD/M
      Variance<-c(Variance, Var)
      Er<-0
      Error<-c(Error, Er)
      }else if (length(interestcol.na)<x && override == "Yes"){
        #Calculate the mean, standard deviation, and variation, but also give a warning
        M1<-mean(interestcol.na)
        SD1<-sd(interestcol.na, na.rm = FALSE)
        Var1<-M1/SD1
        Variance<-c(Variance, Var1)
        Er1<-1
        Error<-c(Error, Er1)
        }else{
          Var2<-0
          Variance<-c(Variance, Var2)
          Er2<-2
          Error<-c(Error, Er2)
        }
    }
  return(data.frame(File, Variance, Error))  
  print("Error 0: No Error, congrats.")
  print("Error 1: Warning: There are not enough observations for a reliable calculation, but this error was overlookeed by your inputs")
  print("Error 2: Error: There are not enough observations, please add more. ")
}
  
