# Advent of code 2017
# Day 15


library(Rcpp)


MAX_INT <- .Machine$integer.max

#Sys.setenv(PATH = paste(Sys.getenv("PATH"), "C:/RBuildTools/3.4/bin/",
#                        "C:/RBuildTools/3.4/mingw_64/bin", sep = ";"))
#Sys.setenv(BINPREF = "C:/RBuildTools/3.4/mingw_64/bin/")
#sourceCpp('pair_gather.cpp')

num2bin <- function(number){
  vec <- rev(as.numeric(intToBits(number)))
  return(vec)
}

compNums <- function(a, b){
  aBin <- num2bin(a)[17:32]
  bBin <- num2bin(b)[17:32]
  val <- 0
  if(identical(aBin,bBin)){
    print("plus one")
    val <- 1
  }
  return(val)
}


#valueGenerator <- function(value, genValue){
#  out <- (value * genValue) % MAX_INT
#}

judge <- function(firstLap, secondLap, generators){
  
  judEnv <- new.env()
  judEnv$count <- 0

  GEN_A <- 16807
  GEN_B <- 48271
  a <- unname(generators['A'])
  b <- unname(generators['B'])
  judEnv$a <- a
  judEnv$b <- b
  # First phase is where the judge compares without any conditions
  lapply(c(1:firstLap),FUN=
    function(x){
      judEnv$a <- (judEnv$a * GEN_A) %% MAX_INT
      judEnv$b <- (judEnv$b * GEN_B) %% MAX_INT
      judEnv$count = judEnv$count + compNums(judEnv$a, judEnv$b)
    }         
  )
  # Phase 1
  print(judEnv$count)
  count <- 0

  samples <- 0
  aData <- c()
  bData <- c()
  # Second phase judge needs to have conditions so we collect the pairs first
  while(samples < secondLap){
    
    if(length(aData) < secondLap){
      a <- (a * GEN_A) %% MAX_INT
      if(a %% 4 == 0){
        aData <- c(aData, a)
        samples = samples + 0.5
        #print("a:")
        #print(length(aData))
      }
    }
    if(length(bData) < secondLap){
      b <- (b * GEN_B) %% MAX_INT
      if(b %% 8 == 0){
        samples = samples + 0.5
        bData <- c(bData, b)
        #print("b:")
        #print(length(bData))
      }
    }
  }
  print("Collection done.")
  judEnv$bData <- bData
  judEnv$index <- 1
  judEnv$count <- 0
  lapply(aData, FUN=
    function(x){
      judEnv$count = judEnv$count + compNums(x, judEnv$bData[judEnv$index])
      judEnv$index = judEnv$index + 1
    }     
  )
  print(judEnv$count)
  #print(bData)
}


main <- function(){
  
  start = Sys.time()
  
  # Reads the commandline argument
  argv <- commandArgs(TRUE)
  
  input <- readLines(argv[1])
  print(input)
  data <- unlist(lapply(input, FUN=
      function(x){
        parts <- unlist(strsplit(x, " "))
        value <- strtoi(parts[length(parts)])
        names(value) <- parts[2]
        out<- value
      }
    )
  )
  judge(5, 5000000, data)
  
  sprintf("Program took: %f", Sys.time()-start)
}

main()