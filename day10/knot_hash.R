# Advent of code 2017
# Day 10
# Inputs are: list of integers and integer

hashCode <- function(lengths, data){
  'Phase 1 function to hash given data according
   to given lengths.'
  
  hashEnv <- new.env()
  hashEnv$data <- data
  skip <- 0
  i <- 1
  
  cycle <- rep(1:length(data))
  indexes <- unlist(lapply(c(1:length(lengths)), FUN=function(x) out <- cycle))
  index <- 1

  repeat{
    # Make reversed slice
    to <- lengths[skip+1]
    temp <- c(hashEnv$data[i:length(data)], hashEnv$data[1:length(data)])
    slice <- rev(temp[1:to])
    
    # Apply the slice to hash data
    hashEnv$index <- i - 1
    sapply(slice, FUN=
      function(x){
        hashEnv$index = hashEnv$index + 1
        if(hashEnv$index > length(data)){
          hashEnv$index <- 1
        }
        hashEnv$data[hashEnv$index] <- x
      }
    )
    # Navigate through indexes 
    index <- index + to + skip 
    i <- indexes[index]
    
    # Increase the skip size
    skip = skip + 1
    
    if(skip == length(lengths))
      break
  }
  return(hashEnv$data)
}


hashASCII <- function(lengths, data){
  
}


main <- function(){
  
  start <- Sys.time()
  
  argv <- commandArgs(TRUE)
  
  lengths <- strtoi(unlist(strsplit(readLines(argv[1]),",")))
  data <- rep(0:strtoi(argv[2]))
  testl <- c(3, 4, 1, 5)
  testd <- rep(0:4)
  
  # Phase 1
  phase1 <- hashCode(lengths,data)
  print(phase1[1] * phase1[2])
  
  phase2 <- hashASCII(lengths, data)
  print(phase2)
  
  sprintf("Program took: %f", Sys.time() - start)
}

main()