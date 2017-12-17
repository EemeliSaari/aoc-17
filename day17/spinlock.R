# Advent of code 2017
# Day 17


main <- function(){
  start = Sys.time()
  
  # Reads the commandline argument
  argv <- commandArgs(TRUE)
  
  input <- readLines(argv[1])
  
  print(input)
  
  sprintf("Program took: %f", Sys.time()-start)
}

main()