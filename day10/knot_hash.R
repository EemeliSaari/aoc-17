# Advent of code 2017
# Day 10
# Inputs are: list of integers and integer


# Using bitops library for bitwise XOR
library(bitops)


hashRound <- function(lengths, data, skip=0, ind=1){
  'Phase 1 function to does one round to given data according
   to given lengths.'
  
  hashEnv <- new.env()
  hashEnv$data <- data
  i <- ind

  lenIndex <- 1
  repeat{
    # Make reversed slice
    to <- lengths[lenIndex]
    temp <- c(hashEnv$data[i:length(data)], hashEnv$data[1:i][-i])
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
    if((i + to + skip) %% length(data) != 0)
      i <- (i + to + skip) %% length(data)
    else
      i <- length(data)

    # Increase the skip size
    skip = skip + 1
    
    lenIndex = lenIndex + 1
    if(lenIndex > length(lengths))
      break
  }
  return(list(hashEnv$data,i,skip))
}


hashASCII <- function(input, data){
  'Generates a knot hash for given ASCII input'
  
  hashEnv <- new.env()
  hashEnv$skip <- 0
  hashEnv$pos <- 1
  hashEnv$index <- 1
  hashEnv$sparse <- data
  suffixVals <- c(17, 31, 73, 47, 23)
  hashData <- c(as.numeric(charToRaw(input)), suffixVals)

  # Run 64 rounds
  hashEnv$i <- 0
  lapply(rep(1:64), FUN=
    function(x){
      hash <- c(hashData[hashEnv$index:length(hashData)],hashData[1:hashEnv$index][-hashEnv$index])
      output <- hashRound(lengths=hashData, data=hashEnv$sparse, skip=hashEnv$skip, ind=hashEnv$pos)
      hashEnv$sparse <- unlist(output[1])
      hashEnv$skip <- unlist(output[3])
      hashEnv$pos <- unlist(output[2])
      hashEnv$index = hashEnv$index + 1
      hashEnv$i = hashEnv$i + 1
      if(hashEnv$index == length(hash))
        hashEnv$index <- 1
    }
  )
  # Do the dense hash and convert to hex
  knotHash <- unlist(lapply(seq(16,length(data), 16), FUN=
    function(x){
      slice <- hashEnv$sparse[(x-15):x]
      out <- as.character(as.hexmode(Reduce(bitwXor, slice)),width=2)
    }
  ))
  out <- paste(knotHash, collapse = "")
}


main <- function(){
  
  start <- Sys.time()
  
  argv <- commandArgs(TRUE)
  
  input <- readLines(argv[1])
  data <- rep(0:strtoi(argv[2]))
  parsedInput <- strtoi(unlist(strsplit(input,",")))
  
  # Phase 1
  phase1 <- hashRound(parsedInput, data)
  print(phase1[[1]][1] * phase1[[1]][2])
  # Phase 2
  phase2 <- hashASCII(input, data)
  print(phase2)
  
  sprintf("Program took: %f", Sys.time() - start)
}

main()
