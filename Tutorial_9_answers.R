#set working directory 
setwd("~/Desktop/r-novice-inflammation/Biocomp_tutorial11/practice_dir")

#this is my answer to this week's assignment 

cov_function <- function(dir, col, nrow=50){ 
  #dir should be a path 
  coV <- 0
  file_list <- list.files(dir)
  for(i in file_list){
    table <- read.table(file= i, header=TRUE, sep=",", stringsAsFactors = FALSE)
    num_rows <- length(table[,col])
    if(num_rows<nrow){
      input <- readline(prompt=paste("Warning: File", i, "has less than 50 observations. Type Y to override. Any other key to return. "))
      if(input!="Y"){
        sd <- sd(table[,col])
        mean <- mean(table[,col])
        coV <- c(coV, sd/mean)
        cov[-0]
        return(coV)
      }
    }else{
      sd <- sd(table[,col])
      mean <- mean(table[,col])
      coV <- c(coV, sd/mean)
      coV[-0]
      return(coV)
    }
  }
}
cov_function(dir="~/Desktop/r-novice-inflammation/Biocomp_tutorial11/practice_dir/", col=4)
