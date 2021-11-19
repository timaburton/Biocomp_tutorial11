
# define the function 
coefficient_of_var<-function(dir,column_num,nrow=50){
  
# set working directory and create CoV vector
  setwd(dir)
  dataset <- list.files(dir)
  CoV_vector <- vector()

# calculate reliable coefficient of variation 
  for(i in dataset){
    i <- read.csv(dataset[i])
    if (column_num > ncol(i)){
      print("not valid -- choose lower column_num")
      }
    else {
      # Report error for file with less than 50 observations
      if(nrow(i) < 50){
        print("error")
        # Give option to override and continue with calculation 
        x <- readline("Would you like to override? Please enter 'yes' or 'no': ")
        if (x == "yes"){
          print("Warning! Your file has less than 50 observations.")
          # Calculate the coefficient of variation with an override
          standard_dev<-sd(i[,column_num])
          average<-mean(i[,col])
          coeff_variation<-standard_dev/average
        } else {
          break
        }
  } else {
      # Calculate the coefficient of variation for file with more than 50 obs
      standard_dev<-sd(i[,column_num])
      average<-mean(i[,column_num])
      coeff_variation<-standard_dev/average
      Cov_vector[i]<-coeff_variation
      }
    }
  } return(CoV_vector)
  }