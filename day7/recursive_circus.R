# Advent of code
# Day7
# Input is set of words

library(methods)


tree <- setRefClass("Tree",
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

sumTree <- setRefClass("SumTree",
  fields = list(data="vector",
                weight="numeric",
                index="numeric"),
  methods = list(
    navigate = function(){
      step(unlist(data[index]))
    },
    step = function(target){
      print(target)
      visit(target)
      a <- lapply(target[-c(1,2)], FUN=
        function(x){
          i <-rep(seq_along(data), sapply(data, length))[match(x, unlist(data))]
          main <- find_weight(data[i])
          temp <- unlist(data[i])
          end <- lapply(temp[-c(1,2)], FUN=
            function(y){
              find_weight(y, ind=i)
            }
          )
          out <- c(main,end)
        }
      )
      new <- ""
      rows <- length(target[-c(1,2)])
      cols <- length(a) / rows
      temp <- sapply(c(1:length(a)), FUN=
        function(x){
          out <- unlist(a[[x]])
          print(sum(out))
          if(length(unique(out[-1])) < length(out))
            new <- x
          out
        }
      )
      print(temp)
    },
    visit = function(target){
      data <<- data[-index]
    },
    find_weight = function(target, ind=length(data)){
      temp <- data[-ind]
      out <- sapply(target, FUN=
        function(x){
          i <- rep(seq_along(temp), sapply(temp, length))[match(x, unlist(temp))]
          out <- unlist(strsplit(unlist(temp[i])[2],""))
          out <- out[-c(1,length(out))]
          return <- strtoi(paste(out, collapse = ''))
        }
      )
    }
  )
)


circus <- function(data){
  
  # First phase
  navObj <- tree(data=data, last="", index=1)
  navObj$navigate()
  bottom <- navObj$last
  print(bottom)
  
  startIndex <- rep(seq_along(data), sapply(data, length))[match(bottom, unlist(data))]
  print(startIndex)
  sumObj <- sumTree(data=data, weight=0, index=startIndex)
  sumObj$navigate()
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
                    }, USE.NAMES = FALSE)
  # Start the program with parsed data
  circus(input)
  
  sprintf("Program took: %f", Sys.time()-start)
}

main()
