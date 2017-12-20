# Advent of code 2017
# Day 19
# Input was a maze


navigate <- function(data){
  'Navigate through the maze'
  
  ind <- c(1, match("|", data[1,]))
  dir <- c(1,0)
  
  collection <- c()

  current <- "|"
  steps <- 0
  repeat{
    if(data[ind[1],ind[2]] %in% LETTERS)
      collection = c(collection, data[ind[1],ind[2]])
    else if(data[ind[1],ind[2]] == "+"){
      # determine if travelling vertically or horizontally
      if(dir[1] != 0){
        slice <- data[ind[1], ((ind[2]-1):(ind[2]+1))]
        dir <- c(0, (-2 + match("-", slice)))
      }
      else{
        slice <- data[((ind[1]-1):(ind[1]+1)), ind[2]]
        dir <- c((-2 + match("|", slice)), 0)
      }
    }
    steps = steps + 1
    # Check if the next is a dead end
    if(data[(ind[1]+dir[1]),(ind[2]+dir[2])] == " ")
      break
    # Move otherwise
    ind <- ind + dir
  }
  # Phase 1
  print(paste(collection, collapse = ""))
  # Phase 2
  print(steps)
}


main <- function(){
  
  start = Sys.time()
  
  # Reads the commandline argument
  argv <- commandArgs(TRUE)
  
  rawData <- readLines(argv[1])
  
  # Convert the txt file into a string matrix
  data <- matrix(data=unlist(strsplit(rawData,"")), 
                 nrow = length(unlist(strsplit(rawData[1],""))), 
                 ncol = length(rawData), byrow = TRUE)

  navigate(data)
  
  sprintf("Program took: %f", Sys.time()-start)
}

main()