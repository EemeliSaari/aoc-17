# Advent of code 2017
# Day 2
# Input is a spreadsheet of integer values.
# Graph is plotted with ggplot2 librarys qplot

library(ggplot2)


checkRow <- function(value, row){
  "Helper function to check which index is evenly dived
   with value in question."
  
  temp = 0
  for(i in 1:length(row)){
    if(row[i] %% value == 0 && row[i] > value){
      temp = temp + i
      break
    }
  }
  index = temp
}


getDivisonSum <- function(df){
  
  sum <- 0
  # Initialize empty dataframe to save each loops time.
  iterTime <- data.frame(Measurement=double(), Iteration=integer())
  
  # Goes through the table using nested for loops
  for(r in 1:nrow(df)){
    start = Sys.time()
    for(i in 1:length(df[r,])){
      
      # Finds the correct index using checkRow function
      # Checks everything in the row but the value in question
      index <- checkRow(df[r,i], as.numeric(df[r,-i]))

      if(index != 0){
        sum = sum + as.numeric(df[r, -i][index]) / df[r,i]
        break
      }
    }
    newRow <- data.frame(Measurement=as.numeric(Sys.time() - start),
                         Iteration=r)
    iterTime <- rbind(iterTime, newRow)
  }
  print(sum)
  vec = iterTime
}


getMinMaxSum <- function(df){
  
  checkSum <- 0
  print(length(sum))
  
  # Loops through row in table
  for(r in 1:nrow(df)){
    row <- df[r,] # Similiar syntax to matlabs df[r,:]
    checkSum = checkSum + (max(row) - min(row))
  }
  print(checkSum)
}


makePlot <- function(df){
  
  plot <- qplot(Iteration, Measurement, data=df,geom="line", ylab="seconds")
  #Plot will be saved as .pdf into the current wd.
  print(plot)
}

main <- function(){
  
  start = Sys.time()
  
  # Reads the commandline argument
  argv <- commandArgs(TRUE)
  
  # Reads the file to a table dataframe
  df = read.table(argv[1], sep='\t')
  
  getMinMaxSum(df)
  timeDf <- getDivisonSum(df)
  makePlot(timeDf)
  
  sprintf("Program took: %f", Sys.time()-start)
  
}

main()