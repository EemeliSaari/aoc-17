# Advent of code 2017
# Day 9
# Input is a stream of text


processProgram <- function(data){
  'Process and parse stream input data'
  
  # New environment to work within lapply
  dataEnv <- new.env()
 
   # Index variable to keep tract inside lapply
  dataEnv$index <- 1
  dataEnv$ignored <- c(0)
  
  original <- unlist(strsplit(data,""))
  # find the indexes of the ignored 
  matches <- gregexpr(pattern = "!", data)[[1]]

  # find the actual indexes to ignore 
  lapply(matches, FUN=
    function(x){
      'Helper function to parse ignores'
      if(dataEnv$ignored[dataEnv$index] != x && !(x %in% dataEnv$ignored)){
        dataEnv$ignored <- c(dataEnv$ignored, x, (x+1))
        dataEnv$index = dataEnv$index + 1
      }
    }
  )
  # Remove the first index which is zero
  dataEnv$ignored <- dataEnv$ignored[-1]
  
  # Reset the index
  dataEnv$index <- 1
  dataEnv$subIndex <- 1
  # initialize the data by removing all the ignored pieces
  dataEnv$data <- rep(NA,(length(original)-length(dataEnv$ignored)))
  lapply(original, FUN=
    function(x){
      'Helper function to create data'
      if(is.na(match((dataEnv$index),dataEnv$ignored))){
        dataEnv$data[dataEnv$subIndex] <- x
        dataEnv$subIndex = dataEnv$subIndex + 1
      }
      dataEnv$index = dataEnv$index + 1
    }
  )

  # Collect garbage:
  pattern <- "<.*?>"
  # Count how many matches for pattern
  m <- gregexpr(pattern = pattern, paste(dataEnv$data, collapse=""))[[1]]
  # Calculate before value 
  before <- length(dataEnv$data) - (length(m)*2)
  parsed <- gsub(pattern=pattern, replacement="", paste(dataEnv$data, collapse=""))
  #Calculate after value
  after <- length(unlist(strsplit(parsed,"")))
  # remove ","
  parsed <- gsub(pattern=",", replacement="", parsed)

  # Find the score:
  dataEnv$group <- 1
  dataEnv$score <- 0
  sapply(unlist(strsplit(parsed, "")), FUN=
    function(x){
      'Helper function to count score'
      if(x == "{"){
        dataEnv$score = dataEnv$score + dataEnv$group
        dataEnv$group = dataEnv$group + 1
      }
      else
        dataEnv$group = dataEnv$group - 1
    }
  )
  # Phase 1
  print(dataEnv$score)
  # Phase 2
  print(before-after)
}


main <- function(){
  
  start <- Sys.time()
  
  argv <- commandArgs(TRUE)
  
  input <- readLines(argv[1])
  processProgram(input)

  sprintf("Program took: %f", Sys.time() - start)
}

main()