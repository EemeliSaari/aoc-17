# Advent of code 2017
# Day 16
# Input was a set of instructions to move list variables around


library(Rcpp)
# Source the .cpp file for R script to use
sourceCpp("whole_dance.cpp")


num2string <- function(vec){
  'Converts numeric vector to string'
  
  out <- paste(unlist(lapply(vec, FUN=
      function(x){
       out <- letters[x]
      }                          
    )
  ), collapse = "")
}


dance <- function(moves, data){
  'Execute the dance moves given to the function and return the 
   last position of the dancers.'
  
  # Convert input to executable actions for cpp code
  moveSeq <- unlist(lapply(moves, FUN=
      function(x){
        parts <- unlist(strsplit(x,""))
        t <- unlist(strsplit(paste(parts[2:length(parts)],collapse=""), "/"))
        switch (parts[1],
          "s" = out <- c(0, strtoi(t[1]), 0),
          "x" = out <- c(1, strtoi(t[1]), strtoi(t[2])),
          "p" = out <- c(2, (match(t[1], letters)), (match(t[2], letters)))
        )
      }                           
    )
  )
  pos <- c(1:16)

  # Phase 1
  first <- danceSeq(pos, moveSeq, 1)
  print(num2string(first))
  
  # Phase 2
  second <- danceSeq(pos, moveSeq, 1000000000)
  print(num2string(second))
}


main <- function(){
  start = Sys.time()
  
  # Reads the commandline argument
  argv <- commandArgs(TRUE)
  
  input <- readLines(argv[1])
  movesData <- unlist(strsplit(input, ","))

  dance(movesData)
  
  sprintf("Program took: %f", Sys.time()-start)
}

main()
