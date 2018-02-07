# Advent of code
# Day7
# Input is set of words

library(methods)


Tree <- setRefClass("Tree",
  fields = list(data="vector",
                last="character",
                index="numeric"),
  methods = list(
    navigate = function(){
      step(unlist(data[index]))
    },
    step = function(target){

      if(is.na(index)){
        return()
      }
      else{
        visit(target)
        return(step(unlist(data[index])))
      }
    },
    visit = function(target){
      last <<- target[1]
      data <<- data[-index]
      search <- rep(seq_along(data), sapply(data, length))
      index <<- search[match(last, unlist(data))]
    }
  )
)

getWeight = function(slice){
  out <- unlist(strsplit(slice[2], ""))
  out <- out[-c(1,length(out))]
  return(strtoi(paste(out, collapse = '')))
}

branchSum <- function(data, nodes, index, sum){

  searchData <- data[-index]
  
  for(node in nodes){
    search <- rep(seq_along(searchData), sapply(searchData, length))
    ind <- search[match(node, unlist(searchData))]
    slice <- unlist(searchData[ind])
    sum = sum + getWeight(slice)
    if(length(slice) > 2)
      sum <- branchSum(searchData, slice[3:length(slice)], ind, sum)
  }
  out <- sum
}

findBalance <- function(data, startPoint){
    index <- startPoint
    repeat{
      slice <- unlist(data[index])
      subSlice <- slice[c(3:length(slice))]
      sums <- sapply(subSlice, FUN=
        function(x){
         out <- branchSum(data, c(x), index, 0)
        }
      )
      # Check if the fault is in the root
      if(length(unlist(table(unlist(sums)))) == 1){
        targetWeigth <- unname(unlist(last)[match(names(sort(table(unlist(last)), decreasing=TRUE)[1]), unlist(last))])
        result <- targetWeigth - strtoi(names(unlist(table(unlist(sums))))) * length(unlist(sums))
        return(result)
      }
      newBranch <- names(unlist(sums)[match(names(sort(table(unlist(sums)))[1]), unlist(sums))])
      data <- data[-index]
      search <- rep(seq_along(data), sapply(data, length))
      index <- search[match(newBranch, unlist(data))]

      if(is.na(index))
        break
      last <- sums
    }     
  }


circus <- function(data){

  # First phase
  navObj <- Tree(data=data, last="", index=1)
  navObj$navigate()
  root <- navObj$last
  print(root)

  # Second phase
  startIndex <- rep(seq_along(data), sapply(data, length))[match(root, unlist(data))]
  print(findBalance(data, startIndex))
}

main <- function(){
  start = Sys.time()
  # Reads the commandline argument
  argv <- commandArgs(TRUE)
  # Parse the input data
  input <- sapply(readLines(argv[1]), FUN= 
    function(x){
      line <- unlist(strsplit(x, " "))
      line <- line[-3]
      if(length(line) > 2)
        line <- sapply(line, FUN=
                function(y){
                  y <- unlist(strsplit(y,","))
                }, USE.NAMES = FALSE)
      else
        line
    }, USE.NAMES = FALSE
  )
  # Start the program with parsed data
  circus(input)
  sprintf("Program took: %f", Sys.time()-start)
}

main()
