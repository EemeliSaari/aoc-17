# Advent of code 2017
# Day 14


library(BMS)
library(igraph, warn.conflicts = FALSE)


calculateGrid <- function(key){
  
  calcEnv <- new.env()
  calcEnv$range <- 128 * 2 + 2
  calcEnv$data <- c()
  calcEnv$arg2 <- "255"
  calcEnv$path <- "../day10/knot_hash.R"
  calcEnv$pattern <- "\\w{3,60}"

  lapply(c(0:127), FUN=
    function(x){
      arg1 <- paste(key, "-", as.character(x), sep = "")
      # Wanted to try out running R scripts remotely from another script.
      # It seems to be the biggest resource consumer and not a good option if 
      # have to repeat the same process multiple times.
      result <- system(paste("RScript", calcEnv$path, paste(c(arg1,calcEnv$arg2),collapse = " ")), 
                       intern = TRUE)
      text <- result[1]
      m <- regexpr(calcEnv$pattern, text, perl = TRUE)

      vec <- hex2bin(regmatches(text, m))

      calcEnv$data = c(calcEnv$data, vec[1:128])
    }
  )

  # Phase 1
  print(sum(calcEnv$data))
  grid <- matrix(calcEnv$data, nrow=128, ncol=128, byrow=TRUE)

  labels <- as.vector(grid)
  g <- graph.lattice(dim(grid))
  #lyt <- layout.auto(g)
  
  edgelist <- get.edgelist(g)
  retain <- labels[edgelist[,1]] == labels[edgelist[,2]]
  g <- delete.edges(g, E(g)[!retain])
  
  groups <- matrix(clusters(g)$membership, nrow=nrow(grid))
  e <- new.env()
  e$i <- 1
  # Parse the zeros out of the graph
  done <- unlist(lapply(as.vector(groups), FUN=function(x){
    val <- 0
    if(labels[e$i] != 0)
    {
      val <- x
    }
    e$i = e$i + 1
    out <- val
  }))
  #Phase 2
  # Ignore the zeros
  print(length(unique(done)) - 1)
  
}


main <- function(){
  start = Sys.time()
  
  # Reads the commandline argument
  argv <- commandArgs(TRUE)

  input <- readLines(argv[1])
  test <- "flqrgnkx"
  calculateGrid(input)
  
  sprintf("Program took: %f", Sys.time()-start)
}

main()