# Advent of code 2017
# Day 3
# Input is 368078


library(methods)


findDistance <- function(number){
  'Finds the manhattan distance to the number in
   question.'
  
  squareSide <- findSide(number)g
  temp <- squareSide - 1
  
  if(squareSide != number){
    
    step = floor(squareSide/2)
    startPoint = squareSide^2
    
    repeat{
      startPoint = startPoint - (step * 2)
      
      if(number == startPoint){
        break
      }
      else if(number > startPoint){
        # Check the distance to the center
        distCenter <- abs(startPoint + step - number)
        temp = temp - (step - distCenter)
        break
      }
    }
  }
  distance <- temp
}


findSide <- function(number){
  'Helper function to search what is the layers squares
   side where the number in question is'  
  
  layer = 2
  side = 3
  
  repeat{
    if(number <= (side+2)^2){
      side = side + 2
      break
    }
    else{
      side = side + 2
      layer = layer + 1
    }
  }
  squareSide = side
}

# Helper class for the algorithm to build the spiral grid and search for the number
spiralGrid <- setRefClass("spiralGrid",
  fields = list(innerLayer="vector", 
                outerLayer="vector", 
                size="numeric", 
                bigger="numeric", 
                target="numeric"),
  # class methods in R are defined like this, they are like the usual function
  methods = list(
    buildLayer = function(){
      'Builds the outerLayer by using the sum of every adjusted tile'
      
      subStep <- length(innerLayer)/4
      step <- length(outerLayer)/4
      
      i <- 1
      subIndex <- 1
      outerIndex <- 1
      # make the initial subSlice of the first square side
      subSlice <- innerLayer[subStep*4]
      # adding zero to the end and to beginning to help indexing
      subSlice <- c(c(0), subSlice, innerLayer[subIndex:subStep], c(0,0))

      carry <- 0
      for(value in outerLayer){
        num <- subSlice[i] + subSlice[i+1] + subSlice[i+2] + carry
        
        # Check which side
        if(i %% step == 0 && i >= step){

          # new subSlice every side
          subIndex = subIndex + subStep
          subSlice <- innerLayer[(subIndex-1):(subIndex+subStep-1)]
      
          subSlice <- c(c(0), subSlice, c(0,0))
          i <- 1
          
          # set carry on each side
          carry = carry + outerLayer[outerIndex-1]
        }
        else{
          carry <- 0
          i = i + 1
        }
        # Ignore at the first index
        if(outerIndex != 1)
          num = num + outerLayer[outerIndex-1]
        
        # Add the fisrt number to second the last
        if(outerIndex == length(outerLayer)-1)
          num = num + outerLayer[1]
        
        # Breaks if the number is found.
        if(num >= target){
          bigger <<- num
          break
        }
        else{
          outerLayer[outerIndex] <<- num
          outerIndex = outerIndex + 1
        }
      }
      
      last <- outerLayer[length(outerLayer)]
      outerLayer[length(outerLayer)] <<- last + outerLayer[1]
    }
  )
)


fromGrid <- function(number){
  'Builds the spiraling grid and finds the next
   biggest number to number in question.'
  
  # This is shameful cheating
  start = c(1,2,4,5,10,11,23,25)
  
  bigger <- 0
  square <- 3
  repeat{
    outer <- (square + 2)^2 - square^2 
    
    #Build the object
    grid <- spiralGrid(innerLayer=start, 
                       outerLayer=rep(0,outer), 
                       size=square+2, 
                       target=number, 
                       bigger= bigger)
    grid$buildLayer()
    
    if(grid$bigger > 0){
      bigger <- grid$bigger
      break
    }
    
    # goes up a layer and sets new layer square to be the old layer
    square = square + 2
    start <- grid$outerLayer
  }
  return <- bigger
}


main <- function(){
  start = Sys.time()
  
  # Reads the commandline argument
  argv <- commandArgs(TRUE)
  
  inputNum = strtoi(argv[1])
  
  print(findDistance(inputNum))
  print(fromGrid(inputNum))
  
  sprintf("Program took: %f", Sys.time()-start)
}

main()