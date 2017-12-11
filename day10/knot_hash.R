# Advent of code 2017
# Day 10
# Inputs are: list of integers and integer

hashCode <- function(lengths, data, skip=0, ind=1){
  'Phase 1 function to hash given data according
   to given lengths.'
  
  hashEnv <- new.env()
  hashEnv$data <- data
  i <- ind
  #print(ind)
  cycle <- rep(1:length(data))
  #indexes <- unlist(lapply(c(1:(length(lengths)+skip*2)), FUN=function(x) out <- cycle))
  index <- 1
  #print(i)
  #print(skip)
  #print(length(indexes))
  lenIndex <- 1
  repeat{
    # Make reversed slice
    #print(lengths[lenIndex])
    to <- lengths[lenIndex]
    #print(hashEnv$data[i:length(data)])
    
    temp <- c(hashEnv$data[i:length(data)], hashEnv$data[1:length(data)])
    #print(temp)
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
    #index <- index + to + skip 
    #print(index)
    #print(to + skip)
    i <- (i + to + skip) %% length(data)
    #i <- indexes[index]
    #print(i)
    
    # Increase the skip size
    skip = skip + 1
    
    lenIndex = lenIndex + 1
    if(lenIndex > length(lengths))
      break
  }
  return(list(hashEnv$data,i,skip))
}


hashASCII <- function(lengths, data){
  
  hashEnv <- new.env()
  hashEnv$skip <- 0
  hashEnv$pos <- 1
  hashEnv$index <- 1
  hashEnv$data <- data
  suffixVals <- c(17, 31, 73, 47, 23)
  hashData <- c(as.numeric(charToRaw(lengths)), suffixVals)
  #print(hashData)
  
  lapply(rep(1:64), FUN=
    function(x){
      hash <- c(hashData[hashEnv$index:length(hashData)],hashData[1:hashEnv$index][-hashEnv$index])
      output <- hashCode(lengths=hash, data=hashEnv$data, skip=hashEnv$skip, ind=hashEnv$pos)
      hashEnv$data <- unlist(output[1])
      hashEnv$skip <- unlist(output[3])
      hashEnv$pos <- unlist(output[2])
      hashEnv$index = hashEnv$index + 1
      if(hashEnv$index == length(hash))
        hashEnv$index <- 1
      #print(hashEnv$index)
    }
  )
  #print(hashEnv$data)
}


main <- function(){
  
  start <- Sys.time()
  
  argv <- commandArgs(TRUE)
  
  input <- readLines(argv[1])
  data <- rep(0:strtoi(argv[2]))
  testl <- c(3,4,1,5)
  testacii <- c("1,2,3")
  testd <- rep(0:4)
  parsedData <- strtoi(unlist(strsplit(input,",")))
  
  # Phase 1
  output <- hashCode(parsedData, data)
  #output <- hashCode(testl,testd)
  print(output[[1]][1] * output[[1]][2])
  #print(output)
  phase2 <- hashASCII(input, data)
  #print(phase2)
  
  sprintf("Program took: %f", Sys.time() - start)
}

main()
