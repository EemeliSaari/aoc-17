# Advent of code
# Day 5
# Input is a list of integers

navigate <- function(data, mode=FALSE){
  'Navigate through the maze ether with
   simple rule mode=FALSE or complex mode=TRUE'
  
  index <- 1
  indexLimit <- length(data)
  steps <- 0
  
  # Loops until gets out of the maze
  repeat{
    if(index > indexLimit)
      break
    
    current <- index
    # Increase the step count every iteration
    steps = steps + 1
    if(data[index] != 0)
      index <- index + data[index]
    
    if(mode){
      if(data[current] >= 3)
        data[current] = data[current] - 1
      else
        data[current] = data[current] + 1
    }
    else
      data[current] = data[current] + 1
  }
  out <- steps
}


main <- function(){
  
  start <- Sys.time()
  
  argv <- commandArgs(TRUE)

  input <- strtoi(readLines(argv[1]))

  print(navigate(input))
  print(navigate(input,mode=TRUE))
  
  sprintf("Program took: %f", Sys.time() - start)
}

main()