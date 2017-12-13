# Advent of code 2017
# Day 12


library(igraph, warn.conflicts = FALSE)


makeNet <- function(rawData, searchTarget){
  
  dataEnv <- new.env()
  dataEnv$linksDf <- data.frame(source=integer(), target=integer())
  dataEnv$nodesDF <- data.frame(name=character())
  
  
  # Parse the raw data and turn it into a data frame
  lapply(rawData, FUN=
    function(x){
      row <- unlist(strsplit(gsub(pattern=",", replacement="", x), " "))
      source <- row[1]
      lapply(row[3:length(row)], FUN=
        function(y){
          newEntry <- data.frame(source=strtoi(source), target=strtoi(y));
          dataEnv$linksDf <- rbind(dataEnv$linksDf, newEntry)
        }
      )
      newEntry <- data.frame(name=source)
      dataEnv$nodesDf <- rbind(dataEnv$nodesDf, newEntry)
    }
  )
  net <- graph_from_data_frame(d=dataEnv$linksDf, vertices=dataEnv$nodesDf)
  net <- simplify(net, remove.multiple = T, remove.loops = T)
  
  V(net)$size <- 1
  V(net)$frame.color <- "white"
  V(net)$color <- "orange"
  V(net)$label <- ""
  E(net)$arrow.mode <- 0
  layout <- layout_in_circle(net)
  plot(net)
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