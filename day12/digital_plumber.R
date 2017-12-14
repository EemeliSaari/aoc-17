# Advent of code 2017
# Day 12

library(methods)
library(igraph, warn.conflicts = FALSE)


network <- setRefClass("Network",
  fields = list(data = "list",
                connections = "list",
                visited = "vector",
                df = "data.frame"),
  methods = list(
    make_network = function(startNode){
      netEnv <- new.env()

      startPos <- find_connections(startNode)
      connections <<- c(connections, unlist(startPos))
      netEnv$pending <- unlist(startPos)

      while(length(netEnv$pending) > 0){
        links <- find_connections(netEnv$pending[1])
        netEnv$pending <- netEnv$pending[-1]
        if(length(links) > 0){
          connections <<- c(connections, links)
          netEnv$pending <- c(netEnv$pending, unlist(links))
        }
      }
    },
    find_connections = function(target){

      visited <<- c(visited, target)

      result <- unname(unlist(data[as.character(target)]))
      e <- new.env()
      e$valid <- list()
      lapply(result, FUN=
        function(x){
          search <- rep(seq_along(connections), sapply(connections, length))
          index <- search[match(x, unlist(connections))]
          if(is.na(match(x, visited)) && is.na(index))
            e$valid <- c(e$valid, x)
        }                
      )
      names(e$valid) <- rep(target,length(e$valid))

      out <- e$valid
    }
  )
)


makeNet <- function(rawData, searchTarget){
  
  dataEnv <- new.env()
  dataEnv$linksDf <- data.frame(source=integer(), target=integer())
  dataEnv$nodesDF <- data.frame(name=character())
  
  # Parse the raw data
  parsed <- lapply(rawData, FUN=
     function(x){
       row <- unlist(strsplit(gsub(pattern=",", replacement="", x), " "))
       out <- mapply(function(x,y) { y }, x=rep(row[1],length(row[3:length(row)])), y=strtoi(row[3:length(row)]))
     }
  )
  names(parsed) <- seq(0,length(rawData)-1,1)
  #print(parsed)
  group <- network(data=parsed)
  group$make_network(searchTarget)

  # Phase 1
  print(length(group$visited))
  links <- data.frame(source=names(group$connections), target=unlist(group$connections))
  nodes <- data.frame(name=unlist(group$visited))

  #dataEnv$parsed <- parsed
  groups <- 1 # since one grop has already been established.
  while(length(parsed) > 0){
    #print(length(parsed))
    #print(unlist(group$connections))
    targets <- lapply(group$visited, FUN=
        function(x){
          out <- as.character(strtoi(x) + 1)
        }
      )
    parsed <- parsed[-unlist(unname(targets))]
    group <- network(data=parsed)
    print(length(parsed))
    #print(names(parsed))
    group$make_network(strtoi(names(parsed)[1]))
    #print(length(group$visited))
  }
  # Phase 2
  print(groups)
}


plotNet <- function(links, nodes){
  
  net <- graph_from_data_frame(d=links, vertices=nodes)
  net <- simplify(net, remove.multiple = T, remove.loops = T)
  
  V(net)$size <- 2
  V(net)$frame.color <- "gray"
  V(net)$color <- "orange"
  V(net)$label <- ""
  E(net)$arrow.mode <- 0
  
  # Save the plot as .jpeg image
  jpeg(filename = 'plot12.jpeg',
      width = 1280, height = 760, units = 'px',
      bg = 'white', quality=100, res = NA)
  #jpeg(filename = 'plot.jpg',)
  plot(net)
  #dev.copy(jpeg,filename="plot.jpg")
  dev.off()
}


main <- function(){
  start = Sys.time()
  
  # Reads the commandline argument
  argv <- commandArgs(TRUE)
  
  data <- readLines(argv[1])
  target <- strtoi(argv[2])
  makeNet(data, target)
  
  sprintf("Program took: %f", Sys.time()-start)
}

main()