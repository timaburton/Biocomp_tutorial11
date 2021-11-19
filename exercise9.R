###define a function called CoE, use it to specified directory, and calculate the coefficient for a user specified column. 
###When file has less than 50 observation, less columns than input number and has NAs in column,give a warning and override it

###name the function,give variation about file, column and override conditions 
CoE <- function(dir,col,is_row_less_50=TRUE,does_stop_wrong_col=TRUE,does_stop_has_na=TRUE){

  
 #get files and file numbers in the directory  
 files <- list.files(path =dir, pattern = "csv", full.names = TRUE)
 filenumber <- vector(mode="numeric",length=length(files))

 ##calculate the coefficient and put results into a vector in its original order
 count <- 0
 for(f in files){
    #record the order of file
    count <- count+1
    #read data in the file
    data <- read.csv(file = f, header = TRUE)
   
    ##if data has fewer column number than user expected, give a warning and save NA to vector in its order
    if (ncol(data)<col){
      #input TURE for does_stop_wrong_col to stop procedure
      if (does_stop_wrong_col==TRUE){
        stop("file does not have the input column")
       #input FALSE for does_stop_wrong_col to give a warning, save NA to the vector in its order
      }else{
        warning("file does not have the input column")
        filenumber[count] <- NA
        #continue calculating the next file
        next
        }
      }
    
     ##get the date of a user specified column
     colume <- data[,col]
     #if the specified column contained NA, give a warning, remove the NA and use other number for calculating
     if (is.na(colume)==TRUE){
       #input TURE for does_stop_has_na to stop procedure
       if (does_stop_has_na){
         stop("file contains NA in its column")
        #input FALSE for does_stop_has_na to give a warning and keep the procedure 
       }else{
         warning("file contains NA in its column")
         }
       }
   
      ##remove NA in the column and use other number for calculating the coefficient
      coefficient <- sd(colume, na.rm=TRUE)/mean(colume, na.rm=TRUE)
   
      #if observation less than 50, give a warning, use all the number for calculating 
      if(length(colume)<50){
        #input TURE for is_row_less_50 to stop procedure
        if(is_row_less_50==TRUE){
          stop("file has less than 50 observations")
        #input FALSE for is_row_less_50 to give a warning and keep the procedure 
        }else{
          warning("file has less than 50 observations")
          }
        }
     
      ##save every coefficient to the vector in its order
      filenumber[count] <- coefficient
    }
  
###return filenumber as the result of CoE 
 return(filenumber)
}



##input the directory, specified column and override condition to get the vector of coefficient
result <- CoE("testing",2,FALSE,FALSE,FALSE)
print(result)