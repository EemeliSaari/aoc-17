# Advent of code 2017
# Day 15
# Input was generator starting values as integers.


library(Rcpp)


# Source the .cpp file for the R script to use
sourceCpp('pair_gather.cpp')


judge <- function(firstLap, secondLap, generators){
  
  GEN_A <- 16807
  GEN_B <- 48271

  a <- unname(generators['A'])
  b <- unname(generators['B'])

  # Phase 1
  first <- firstSampling(a, b, firstLap)
  print(first)
  
  # Phase 2
  second <- secondSampling(a, b, secondLap)
  print(second)
}


main <- function(){
  
  start = Sys.time()
  
  # Reads the commandline argument
  argv <- commandArgs(TRUE)
  
  input <- readLines(argv[1])
  data <- unlist(lapply(input, FUN=
      function(x){
        parts <- unlist(strsplit(x, " "))
        value <- strtoi(parts[length(parts)])
        names(value) <- parts[2]
        out<- value
      }
    )
  )
  judge(40000000, 5000000, data)
  
  sprintf("Program took: %f", Sys.time()-start)
}

main()