# function cvar that can return the coefficient_variation of selected column in every *.cvs files in a selected directory.
#   dir -- directory. 
#   select_col -- selected column name
#   z -- reliable observation


rm(list = ls())
setwd("/Users/lillian/Desktop/Biocomp_tutorial11/")

cvar=function(dir = "test", select_col = c("wage"), z = 50){
  data_files = list.files(dir,
                          full.names = TRUE)
  results <- data.frame(file_name = vector(mode = "character", length = length(data_files)),
                        coefficient_variation = vector(mode = "numeric", length = length(data_files)),
                        file_message = vector(mode = "character", length = length(data_files)))
  for (i in 1:length(data_files)){
    filename <- data_files[i]
    data <- read.csv(filename)
    y <- tryCatch(length(data[ , select_col]), error= function(e) {return(0)}  )
    if (y == 0) {
      cv = c("")
      mes = c("column not exist")
      results$file_name[i] <- filename
      results$coefficient_variation[i] <- cv
      results$file_message[i] <- mes
      next}
    
    datacol=data[ , select_col]
    len_selectcol = length(datacol)  
    if(len_selectcol >= z){
      cv = sd(datacol, na.rm=T) / mean(datacol, na.rm=T)
      mes = c("meet criteria")
      results$file_name[i] <- filename
      results$coefficient_variation[i] <- cv
      results$file_message[i] <- mes
    }else if(len_selectcol < z){
      cv = sd(datacol, na.rm=T) / mean(datacol, na.rm=T)
      mes = c("warning: less than reliable observations")
      results$file_name[i] <- filename
      results$coefficient_variation[i] <- cv
      results$file_message[i] <- mes
    }
  }
  print(results)
}