# Advent of code
# Day 6
# Input is list of integers size of 16


allocateMemory <- function(block){

  # Finds the index of the biggest number  
  index <- match(max(block), block)
  value <- block[index]

  # Determin the carry number to be added to the empty spot
  if(value %% (length(block) - 1) == 0)
    carry <- 0
  else
  {
    if(value < length(block))
    {
      carry <- 0   
    }
    else
    {
      carry <- 1
      value = value - 1 
    }
  }
  
  # Reallocates the value to other values in the vector.
  block[index] <- 0
  i <- index
  while(TRUE){

    if(i != index)
    {
      if(value == 0)
        break
      block[i] <- block[i] + 1
      value = value - 1
    }
    i = i + 1
    if(i > length(block))
      i <- 1
  }
  block[index] <- carry
  out <- block
}


allocateProgram <- function(startBlock){
  
  # Using a list of vectors
  df <- list(startBlock)
  lastBlock <- startBlock
  i <- 1
  # Allocates the memory blocks until duplicate is found
  while(TRUE){
    newEntry <- allocateMemory(lastBlock)
    # checks for matches in the list of vectors
    if(Position(function (x) identical(x, newEntry), df, nomatch = 0) > 0){
      # Saves the last entry for further usage
      target <- newEntry
      break
    }
    else
      df[[i]] <- newEntry
    
    lastBlock <- newEntry
    i = i + 1
  }
  # First phase:
  print(i)
  
  # After that, check when the target block entry appears again.
  count <- 1
  lastBlock <- target
  while(TRUE){
    newEntry <- allocateMemory(lastBlock)
    if(identical(newEntry, target))
      break
    
    count = count + 1
    lastBlock <- newEntry
  }
  # Second phase:
  print(count)
}


main <- function(){
  
  start = Sys.time()
  
  # Reads the commandline argument
  argv <- commandArgs(TRUE)
  
  # Parse and convert string to integer input
  input <- strtoi(unlist(strsplit(readLines(argv[1]),"\t")))

  allocateProgram(input)
  sprintf("Program took: %f", Sys.time()-start)
}

main()