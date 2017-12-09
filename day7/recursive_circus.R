# Advent of code
# Day7
# Input is set of words with recursive properties

library(methods)


tree <- setRefClass("Tree",
  fields = list(data="vector",
                #visited="vector",
                index="numeric"),
  methods = list(
    navigate = function(){
      step(data[index])
    },
    step = function(target){
      visit(target)
      search <- rep(seq_along(data), sapply(data, length))
      index <<- search[match(target[1], unlist(data))]
      step(data[index])
    },
    visit = function(target){
      data <<- data[-index]
      print(target)
      
      #if(target > 2){
      #  target <- target[-c(1,2,3)]
      #  search <- rep(seq_along(data), sapply(data, length))
      #  tagets <- search[match(parts[1], unlist(data))]
      #}
    }
  )
)


main <- function(){
  start = Sys.time()
  
  # Reads the commandline argument
  argv <- commandArgs(TRUE)

  input <- strsplit(readLines(argv[1])," ")
  #print(input[188])
  #print(strsplit(input[188]," ")[[1]][1])
  t <- input[188]#"keztg"
  t <- unlist(t)[-c(1,2,3)]
  print(t)
  #print(input)
  g <- rep(seq_along(input), sapply(input, length))
  print(g[match(t, unlist(input))])
  o <- tree(data=input,index=1)
  o$navigate()
  sprintf("Program took: %f", Sys.time()-start)
}

main()
