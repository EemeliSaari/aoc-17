# Advent of code 2017
# Day 1
# Input is a list of integers from 1-9.
# Input list is treated as circular list.


inverseCaptcha <- function(vec, index){
  
  #initialize the loop index and sum
  sum <- 0
  i <- 1
  # Cool R loop
  repeat{
    num <- vec[i]
    
    # since circular circular list we have to check
    # decide the right indexing
    if(i + index > length(vec)){
      newIndex <- index - (length(vec) - i)
      comp <- vec[newIndex]
    }
    else{
      comp <- vec[i + index]  
    }
    
    # Does the comparing and adds to the sum
    if(identical(num, comp)){
      sum = sum + vec[i]
    }
    
    i = i + 1
    if(i == length(vec) + 1){
      break
    }
  }
  print(sum)
}


main <- function(){
  start <- Sys.time()
  
  # Checks the current working directory
  WD <- getwd()
  
  # Sets working directory to data folder if not
  # already.
  if(!grepl("data", WD))
    setwd("data/")

  # Reads the input file to string
  DATA = readLines('input1.txt')
  
  # splits eachs character and converts them to integers vector
  data = strtoi(strsplit(DATA, "")[[1]])
  
  # First task asked to find the next index which is 1
  task1index <- 1
  
  # Second task asked to find the index halfway around
  task2index <- length(data) / 2
  
  inverseCaptcha(data, task1index)
  inverseCaptcha(data, task2index)
  
  sprintf("Program took: %f", Sys.time()-start)
}

main()