# Hope Baldwin
# hbaldwi2@nd.edu
# Exercise 9

# calculate coefficient of variation (standard dev / mean) for the column they specified
getCOVs<-function(dir, colNum){
  
  # empty vector to store the COVs
  covs<-c()
  
  # get all the files in the directory
  files <-list.files(path=dir)
  
  # for each of the files, add the full path name 
  for(i in 1:length(files)){
    files[i] = paste(dir,files[i],sep="/")
    
    #read the data in from the files
    data= read.csv(files[i], header=TRUE)
    
    # if there are less than 50 rows give a warning 
    if(nrow(data) < 50){
      warning("Less than 50 data points in file.")
    }
    
    # if the column exists for that file, calculate the COV and store in vector 
    if(ncol(data) >= colNum){
      cov<-(sd(data[,colNum], na.rm = TRUE)/mean(data[,colNum], na.rm = TRUE))
      covs<-append(covs,cov)
    }else{
      cat("Column number does not exist in the file ", files[i],"\n")
    }
  }
  
  # return the coefficient of variations in a vector
  return(covs)
}

