# Exercise 9 

#write a function to read data from file and calculate
#coefficient of variation

directoryVariation <- function(directory, column){
  #set directory
  setwd(directory)
  #create a list of files from target directory
  file_list <- list.files(path = ".", full.names = TRUE)
  #create data frames for output
  dataset <- data.frame()
  final <- data.frame()
  temp_data <- data.frame()
  #read files and calculate variable
  for (i in 1:length(file_list)){
    temp_data <- read.csv(file_list[i], header = TRUE, stringsAsFactors = FALSE)
    if (nrow(temp_data) > 50) {
      dataset <- sd(temp_data$column) / mean(temp_data$column) * 100
      #for each cycle of the loop, bind the new data to the building dataset
      final <- rbind(dataset, final)
    }
    else if (nrow(temp_data) < 50) {
      print ("Recommended 50 observations not reached for a file") 
      input <- readline(prompt = "Override? (y/n): ")
      if (input=="y"){
        dataset <- sd(temp_data$column) / mean(temp_data$column) * 100
        #for each cycle of the loop, bind the new data to the building dataset
        final <- rbind(dataset, final)
      } else {
        dataset <- "N/A"
        #for each cycle of the loop, bind the new data to the building dataset
        final <- rbind(dataset, final)
      }
    }
  }
}
