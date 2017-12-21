# Advent of code 2017
# Day 20


library(methods)


manhattanDist <- function(vec){
  return(sum(abs(vec)))
}


isInt <- function(x){
  return(x %% 1 == 0)
}


isValid <- function(roots){
  r <- TRUE

  valued <- list()
  for(root in roots){

    comp <- root[root %% 1 == 0]
    comp <- comp[comp > 0]
    nans <- comp[is.na(comp)]
    infs <- comp[is.infinite(comp)]

    if(length(comp) > 0 && length(nans) != 2  && length(infs) != 2){
      valued = c(valued, list(root))
    }
  }
  if(length(valued) != length(roots)){
    r <- FALSE
  }
  return(r)
}


quadraticEq <- function(pos, vel, acc){
  r <- 0
  dis <- unlist(lapply(c(1:length(pos)), FUN=
      function(x){
        out <- vel[x]^2 - (4 * pos[x] * acc[x])
      }
    )
  )
  # Check if it's valid to get any real roots
  if(length(dis[dis > 0]) == length(dis)){
    roots <- lapply(c(1:length(pos)), FUN=
      function(x){
        x1 <- (-vel[x] + sqrt(dis[x])) / (2*acc[x]) 
        x2 <- (-vel[x] - sqrt(dis[x])) / (2*acc[x])
        out <- c(x1,x2)
      }
    )
    # Check if any valid roots available that are positivite
    # integers.
    if(isValid(roots)){
      r <- Reduce(intersect, roots)
    }
  }
  return(r)
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
    move = function(time){
      e <- new.env()
      e$i <- 1
      newPos <- unlist(lapply(c(1:3), FUN=
        function(p){
          out <- origin[e$i] + time * velocity[e$i] + acceleration[e$i] * time^2
          e$i = e$i + 1
          out
        }
      ))
      current <<- newPos
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
  dists <- unlist(lapply(particles, FUN=
      function(x){
        x$move(500)
        comp <- manhattanDist(x$current)
      }
    )
  )
  # Phase 1
  print(match(min(dists), dists)-1)


  collisions <- list()
  i <- 1
  for(p in particles){
    subInd <- 1
    for(subP in particles){
      pos <- p$origin-subP$origin
      acc <- (p$acceleration-subP$acceleration)/2
      vel <- (p$velocity-subP$velocity) + acc

      realRoot <- quadraticEq(pos=pos, vel=vel, acc=acc)
      if(realRoot > 0){
        if(!(realRoot %in% names(collisions)))
          collisions[[as.character(realRoot)]] <- c(p$id, subP$id)
        else{
          search <- rep(seq_along(collisions), sapply(collisions, length))
          index <- search[match(p$id, unlist(collisions))]
          
          collisions[[as.character(realRoot)]] <- c(collisions[[as.character(realRoot)]], c(p$id, subP$id))
        }
      }
      
      subInd = subInd + 1
    }
    i = i + 1
  }
  print(collisions)
}

main <- function(){
  
  start = Sys.time()
  
  # Reads the commandline argument
  argv <- commandArgs(TRUE)
  
  rawData <- readLines(argv[1])
  
  controller(rawData)
  
  sprintf("Program took: %f", Sys.time()-start)
}

main()