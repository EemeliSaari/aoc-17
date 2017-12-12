# Advent of code 2017
# Day 11
# Input is set of instructions

# Coord: (y, x, z)

calcDist <- function(to){
  'Calculate distance in hex grid'
  return(max(abs(to)))
}


navigate <- function(route, start){
  'Navigate to the location in hex grid'
  
  world <- new.env()
  world$grid <- start
  world$edge <- 0
  
  lapply(route, FUN=
    function(x){
      world$grid = world$grid + x
      if(max(abs(world$grid)) > world$edge)
        world$edge <- max(abs(world$grid))
    }         
  )
  return(list(world$grid, world$edge))
}


main <- function(){
  start = Sys.time()
  
  # Reads the commandline argument
  argv <- commandArgs(TRUE)
  rawData <- readLines(argv[1])
  testData <- c("se,sw,se,sw,sw") 
  # Parse data
  data <- lapply(unlist(strsplit(rawData, ",")), FUN=
    function(x){
      'Convert the instructions to represent actual move
       in the coordinates'
      
      switch (x,
        "n" = out <- c(1, 0, -1),
        "ne" = out <- c(0, 1, -1),
        "se" = out <- c(-1, 1, 0),
        "s" = out <- c(-1, 0, 1),
        "sw" = out <- c(0, -1, 1),
        "nw" = out <- c(1, -1, 0)
      )
    }
  )
  startLoc <- c(0,0,0)

  # Navigate to the location
  output <- navigate(route=data, start=startLoc)
  location <- output[[1]]
  
  # Phase 1
  dist <- calcDist(location)
  print(dist)
  
  # Phase 2
  furthest <- output[[2]]
  print(furthest)
  
  sprintf("Program took: %f", Sys.time()-start)
}

main()