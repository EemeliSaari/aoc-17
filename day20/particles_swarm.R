# Advent of code 2017
# Day 20
# Input was a list of particle details (position, velocity, acceleration)


library(methods)


manhattanDist <- function(vec){
  return(sum(abs(vec)))
}


# Paritcle class for simulation approach
Particle <- setRefClass("Particle",
  fields = list(
    origin = "vector",
    velocity = "vector",
    acceleration = "vector",
    current = "vector",
    id = "numeric"
  ),
  methods = list(
    move = function(time=1, perm=FALSE){
      if(perm){
        velocity <<- velocity + acceleration
        origin <<- origin + velocity
      }
      else{
        current <<- origin + time*velocity + 0.5 * acceleration * time^2
      }
    }
  )
)


controller <- function(rawData){
  
  # Parse the data with regex
  pattern <- "-?[0-9]*,-?[0-9]*,-?[0-9]*"
  contE <- new.env()
  contE$id <- 1
  particles <- lapply(rawData, FUN=
    function(x){
      m <- gregexpr(pattern, x)
      subStr <- regmatches(x, m)[[1]]
      parts <- lapply(subStr, FUN=
        function(y){
          out <- strtoi(unlist(strsplit(y,",")))
        }
      )
      out <- Particle(origin = parts[[1]], velocity = parts[[2]], acceleration = parts[[3]], id = contE$id)
      contE$id = contE$id + 1
      out
    }
  )
  # In long run (500 tics) the particles are speeding away from the origo.
  dists <- unlist(lapply(particles, FUN=
      function(x){
        x$move(time=500)
        comp <- manhattanDist(x$current)
      }
    )
  )
  # Phase 1
  print(match(min(dists), dists)-1)
  particles <- unlist(particles)
  for(i in c(1:50)){
    # Collect the positions to a list
    positions <- lapply(particles, FUN=
      function(x){
        x$move(perm=TRUE)
        out <- x$origin
      }                      
    )
    # Check the duplicates
    dups <- unique(which(duplicated(positions)))

    if(length(dups) > 0){
      org <- positions
      org[dups] <- 0
      
      comp <- unique(lapply(dups, FUN=function(x) if(!is.null(x)) unlist(positions[x])))
      index <- 1
      rmv <- c()
      # Remove also the original duplicates
      for(var in org){
        res <- unlist(lapply(comp, function(x) if(identical(x,var)) 1 else 0))
        if(1 %in% res)
          rmv = c(rmv, index)
        index = index + 1
      }

      dups <- c(dups, unique(unlist(rmv)))
      particles = particles[-unique(dups)]
    }
  }
  # Phase 2
  print(length(particles))
}


main <- function(){
  
  start = Sys.time()
  
  # Reads the commandline argument
  argv <- commandArgs(TRUE)
  
  controller(readLines(argv[1]))
  
  sprintf("Program took: %f", Sys.time()-start)
}

main()
