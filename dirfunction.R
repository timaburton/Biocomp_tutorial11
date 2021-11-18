#create function that takes directory name as "dir" argument
dirfunction<-function(dir){
# read data from each file 
# calculate coefficient of variation for specific column (return as vector)
  read_data <- list.files(dir)
  coefficient_of_var <- vector()
  column_num <- readline("Select specific column")
  column_num <- as.integer(column_num)
  for(i in 1:length(read_data)){
    setwd(dir)
    data1 <- read.csv(read_data[i])
    if (column_num > NCOL(data1)){
      readline("")
    }
  }
# Report error for file less than 50 observations
  if(length(read_data) < 50){
    print("error")
    
  }
}