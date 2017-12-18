# Advent of code 2017
# Day 17
# Puzzle input was integer 304


openLock <- function(buffer, times){
  'In the first task we need to find the first number
   after 2017, so need to gather numbers produced by
   the spinlock into a vector.'
  
  l <- new.env()
  l$cList <- c(0)
  l$index <- 1
  lapply(c(1:times), FUN=
    function(x){
      if((l$index + buffer) %% length(l$cList) == 0)
        newIndex <- length(l$cList)
      else
        newIndex <- (l$index + buffer) %% length(l$cList) 
      l$cList = append(l$cList, x, after=newIndex)
      l$index = newIndex + 1
    }         
  )
  ind <- match(2017,l$cList)
  return(l$cList[(ind+1)])
}


openCircuit <- function(buffer, times){
  'In the second task we need to find the number
   after 0, so keeping track of only that.'
  
  l <- new.env()
  l$value <- 0
  l$index <- 1
  lapply(c(1:times), FUN=
    function(x){
      if((l$index + buffer) %% x == 0){
        l$value <- x
      }
      l$index <- ((l$index + buffer) %% x) + 1
    }         
  )
  return(l$value)
}


main <- function(){
  start = Sys.time()
  
  # Reads the commandline argument
  argv <- commandArgs(TRUE)
  
  input <- strtoi(argv[1])
  
  print(openLock(input, 2017))
  print(openCircuit(input, 50000000))
  
  sprintf("Program took: %f", Sys.time()-start)
}

main()