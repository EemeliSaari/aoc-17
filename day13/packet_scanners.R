# Advent of code 2017
# Day 13


navigateThrough <- function(data, delay=0){

  names <- strtoi(names(data))
  range <- max(names)
  
  navEnv <- new.env()
  navEnv$data <- data
  navEnv$severity <- 0
  navEnv$catch <- 0
  lapply(c(0:range), FUN=
    function(x){
      if(!(is.na(match(x, names)))){
        layer <- unlist(unname(navEnv$data[as.character(x)]))
        danger <- unname((layer['depth'] - 1)) * 2
        if((x + delay) %% danger == 0){
          navEnv$severity = navEnv$severity + (unname(layer['depth']) * x)
          navEnv$catch = navEnv$catch + 1
        }
      }
    }
  )
  return(list(navEnv$severity,navEnv$catch))
}


calculateTrip <- function(data){

  # Phase 1
  out <- navigateThrough(data)
  print(unlist(out[1]))

  delay <- 0
  repeat{
    result <- navigateThrough(data, delay)
    if(result[2] == 0)
      break
    delay = delay + 1
  }
  # Phase 2
  print(delay)
}


main <- function(){
  start = Sys.time()
  
  # Reads the commandline argument
  argv <- commandArgs(TRUE)
  
  input <- readLines(argv[1])
  dataEnv <- new.env()
  
  # Parse the data
  dataEnv$layers <- rep(0,length(input))
  dataEnv$data <- list()
  dataEnv$index <- 1
  lapply(input, FUN=
    function(x){
      row <- unlist(strsplit(x,": "))
      dataEnv$layers[dataEnv$index] <- row[1]
      dataEnv$data <- c(dataEnv$data, list(list("depth"=strtoi(row[2]))))
      dataEnv$index = dataEnv$index + 1
    }         
  )
  names(dataEnv$data) <- dataEnv$layers
  calculateTrip(dataEnv$data)
  
  sprintf("Program took: %f", Sys.time()-start)
}

main()