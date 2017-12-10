# Advent of code 2017
# Day 8
# Input was list of string instructions


registerProgram <- function(data){
  
  # set list and value variables to different environment
  # so during iteration we can modify them.
  dataEnv <- new.env()
  dataEnv$df <- list()
  valueEnv <- new.env()
  valueEnv$value <- 0
  
  lapply(data, FUN=
    function(x){
      parts <- unlist(strsplit(x," "))
      target <- parts[1]
      comp <- parts[5]
      if(!(comp %in% names(dataEnv$df)))
        dataEnv$df[comp] <- 0
      value <- 0
      
      # make a condition statement string
      cond <- paste(c("dataEnv$df['",comp,"'] ",parts[6]," ",parts[7]), collapse = '')
      # much like the syntax in Python to run string as statement
      if(eval(parse(text=cond)))
        value <- strtoi(parts[3])
      
      # Check if increasing or decreasing
      if(parts[2] == "dec")
        value = value * (-1)
      
      # add the value to list
      if(!(target %in% names(dataEnv$df)))
        dataEnv$df[target] <- value
      else
        dataEnv$df[target] <- unlist(dataEnv$df[target]) + value
      
      # Keeps the track of the highest value.
      maximum <- max(unlist(dataEnv$df))
      if(maximum > valueEnv$value)
        valueEnv$value <- maximum 
    }
  )
  # Phase 1
  print(max(unlist(dataEnv$df)))
  # Phase 2
  print(valueEnv$value)
}


main <- function(){
  
  start = Sys.time()
  
  # Reads the commandline argument
  argv <- commandArgs(TRUE)
  
  input <- readLines(argv[1])
  registerProgram(input)
  
  # Time with the second phase is miserable
  sprintf("Program took: %f", Sys.time()-start)
}

main()