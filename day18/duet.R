# Advent of code 2017
# Day 18
# Input is a set of assembly like instructions


library(methods)


Program <- setRefClass("Program",
  fields = list(
    index = "numeric",
    registers = "list",
    pending = "vector",
    receiving = "character",
    status = "logical",
    id = "numeric",
    lock = "logical",
    count = "numeric",
    sended = "numeric"
  ),
  methods = list(
    step = function(line){
      'Execute the command'
      
      var <- TRUE
      command <- line[1]
      
      if(is.na(suppressWarnings(as.numeric(line[2])))){
        if(!(line[2] %in% names(registers))){
          if(!identical(line[2], "p"))
            temp <- c(0)
          else
            temp <- c(id)
          names(temp) = line[2]
          registers <<- c(registers, temp)
        }
        sValue <- unname(unlist(registers[line[2]]))
      }
      else
        sValue <- as.numeric(line[2])
        
      if(length(line) > 2){
        if(is.na(suppressWarnings(as.numeric(line[3])))){
          if(!(line[3] %in% names(registers))){
            temp <- c(0)
            names(temp) = line[3]
            registers <<- c(registers, temp)
          }
          tValue <- unname(unlist(registers[line[3]]))
        }
        else
          tValue <- as.numeric(line[3])
      }
      switch (command,
        "snd" = 
          pending <<- c(pending, sValue),
        "set" = 
          registers[line[2]] <<- tValue,
        "add" = 
          registers[line[2]] <<- sValue + tValue,
        "mul" = 
          registers[line[2]] <<- sValue * tValue,
        "mod" = 
          registers[line[2]] <<- sValue %% tValue,
        "rcv" = 
          if(TRUE){
            status <<- FALSE
            receiving <<- line[2]
          },
        "jgz" = 
          if(sValue > 0){
            index <<- index + tValue
            var <- FALSE
          }
      )
      if(var)
        index <<- index + 1
    },
    receive = function(package){
      'Receive the package'
      
      if(is.na(package))
        lock <<- TRUE
      else{
        registers[receiving] <<- package 
        status <<- TRUE
      }
    },
    send = function(){
      'Send the package'
      
      if(length(pending) > 0)
        count <<- count + 1

      sended <<- pending[1]
      pending <<- pending[-1]
    }
  )
)


coordinator <- function(data){
  
  # Phase 1
  p <- Program(index=1, status=TRUE, id=0, lock=FALSE, sended=0) 
  while(p$status){

    line <- unlist(strsplit(data[p$index], " "))
    p$step(line)
    if(p$index > length(data))
      break
  }
  print(p$pending[length(p$pending)])
  
  # Phase 2
  p0 <- Program(index=1, status=TRUE, id=0, lock=FALSE, count=0)
  p1 <- Program(index=1, status=TRUE, id=1, lock=FALSE, count=0)
  repeat{
    
    while(p0$status){
      line <- unlist(strsplit(data[p0$index], " "))
      p0$step(line)
    }
    while(p1$status){
      line <- unlist(strsplit(data[p1$index], " "))
      p1$step(line)
    }
    
    if(!is.null(p0$pending)){
      p0$send()
      p1$receive(p0$sended)
    }
    if(!is.null(p1$pending)){
      p1$send()
      p0$receive(p1$sended)
    }
  
    if(p0$lock && p1$lock)
      break
  }
  print(p1$count)
}


main <- function(){
  start = Sys.time()
  
  # Reads the commandline argument
  argv <- commandArgs(TRUE)
  
  rawData <- readLines(argv[1])
  coordinator(rawData)
  
  sprintf("Program took: %f", Sys.time()-start)
}

main()
