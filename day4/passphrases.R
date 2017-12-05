# Advent of code 2017
# Day 4
# Input was list of passphrase words.

library(ggplot2)


validateCombination <- function(line){
  'Check the line for anagram combinations,
   returns 1 if none were found, 0 otherwise.'
  
  original <- line
  # Checks the phase 1 correct lines only
  if(validateLine(line) > 0){
    # Goes through every word in the line
    sortedLine <- sapply(line, FUN=function(x){
      out <- sapply(lapply(strsplit(x,""), FUN=sort), paste, collapse="", USE.NAMES = FALSE)
    }, USE.NAMES = FALSE)
    return <- validateLine(sortedLine)
  }
  else
    return <- 0
}


validateLine <- function(line){
  'Check if there are same words in the line,
   returns 1 if not, 0 otherwise.'
  
  value <- 0
  if(length(line) == length(unique(line)))
     value = 1
  return <- value
}


validateData <- function(data){
  'Validates the each line in the data'
  
  # How many lines without the same word in it.
  resultOne <- print(sum(sapply(strsplit(data, " "), FUN=validateLine, USE.NAMES = FALSE)))
  # How many lines without anagram words in it.
  resultTwo <- print(sum(sapply(strsplit(data, " "), FUN=validateCombination, USE.NAMES=FALSE)))
}


makeHist <- function(data){
  'Makes a simple histogram of the letters in input data'
  
  longVector <- unlist(sapply(strsplit(data, " "), 
    # Split and sorts every word
    FUN=function(x){
      out <- strsplit(x,"")
    }, 
    USE.NAMES = FALSE)
  )
  df <- table(longVector)

  barplot(df, space=1, names.arg = names(df), cex.names = 0.7, col = "red")
}


main <- function(){
  
  start = Sys.time()
  
  # Reads the commandline argument
  argv <- commandArgs(TRUE)
  
  input <- readLines(argv[1])
  validateData(input)
  #makeHist(input)
    
  # Time with the second phase is miserable
  sprintf("Program took: %f", Sys.time()-start)
}

main()