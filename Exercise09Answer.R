#set working directory
setwd("C:Desktop/R Studio Projects/Biocomp_tutorial11")

# define the function 
cov_function <-function(dir,column_num,nrow=50){
  data <- list.files(dir)
  cov <- 0
  
  for(i in data){
    table <- read.table(file= i, header=TRUE, sep=",", stringsAsFactors=FALSE)
    rows <- length(table[,col])
    if(rows<nrow){
      input <- readline(prompt=paste("Warning: File has less than 50 observations. Type X to override. Press any other key to go back."))
      if(input!="X"){
        sd <- sd(table[,col])
        mean <- mean(table[,col])
        cov <- c(cov, sd/mean)
        cov[-0]
        return(cov)
      }
    }else{
      sd <- sd(table[,col])
      mean <- mean(table[,col])
      cov <- c(cov, sd/mean)
      cov[-0]
      return(cov)
    }
  }
}

cov_function(dir="C:Desktop/R studio Projects/Biocomp_tutorial11/", col=4)