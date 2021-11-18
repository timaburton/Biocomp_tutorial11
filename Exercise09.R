## Exercise 9 ##
## Pulling al of the R programming pieces together ##

#Create function to calculate coeffecient variance of separate files
covar = function(dir, colNo){
  covs = 0
  path = list.files(dir) ##list all the files in the directory
  
  # read through each file in directory
  for (i in 1:length(path)){
    file = paste0(dir,path[i]) ## create a full path name to read in the file in the next line
    info = read.table(file, header = T, sep=",", stringsAsFactors = F) ## read in tabular data
  
    #see if file has less than 50 rows/observations and warn user if so
    if (length(info[,colNo]) < 50){
      print(paste0("Warning: file ", file[i]," has less 50 oberservations")) 

      #take in the column specified by the user and calculate coefficient of variation
      covs [i]= (sd(info[,colNo])/mean(info[,colNo]))*100
      
    } else {
      
      #take in the column specified by the user and calculate coefficient of variation
      covs [i]= (sd(info[,colNo])/mean(info[,colNo]))*100
    }
  }
  return(covs) #return the coeffecient of variance values from each file
}

print("Enter the directory in which you would like to read files from. Be sure to include a '/' at the end of the directory name.")
userDir = readline(prompt = "Directory: ")
fileCol = as.integer(readline(prompt = "Which column do you want to find the coeffecient of variance for? "))
covar(userDir, fileCol) # call function
