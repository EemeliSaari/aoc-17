# Advent of code 2017
# Day 21
# Input is a set of rules for enhancing 


patt2mat <- function(patt){
  'Convert string pattern into matrix'
  
  r <- unlist(strsplit(patt, "/"))
  return(matrix(unlist(strsplit(r, "")), length(r), byrow = TRUE))
}


mat2patt <- function(mat){
  'Convert matrix to a string pattern'
  
  r <- unlist(lapply(unname(split(mat,row(mat))), function(x) paste(x, collapse="")))
  return(paste(r,collapse="/"))
}


matSplit <- function(mat, n){
  'Split matrix in to smaller sub parts'
  
  if(nrow(mat) %/% n == 1)
    return(list(mat))
  else{
    lMat <- list()
    indexes <- seq(1, nrow(mat), n)
    for(i in indexes)
      lMat <- c(lMat, lapply(indexes, function(x) mat[(i:(i+n-1)),(x:(x+n-1))]))
    return(lMat)
  }
}


matJoin <- function(mat){
  'Joins a list of matrices into a one square matrix'
  
  if(length(mat) == 1)
    return(do.call(rbind, mat))
  else{
    cMat <- list()
    div <- sqrt(length(mat))
    for(i in seq(1,length(mat), div))
      cMat <- c(cMat, list(do.call(cbind, mat[i:(i+div - 1)])))
    return(do.call(rbind, cMat))
  }
}


# Simple function to rotate matrix
matRot <- function(mat) t(apply(mat, 2, rev))


enhance <- function(data, iter){
  'Enhance the pattern with data rules'
  
  patt <- ".#./..#/###"
  # Convert the raw data to named vector
  rules <- unlist(lapply(data, function(x){
        parts <- unlist(strsplit(x, " => "))
        out <- parts[1]
        names(out) <- parts[2]
        out
      }
    )
  )
  for(i in c(1:iter)){
    mat <- patt2mat(patt)
    if(nrow(mat) %% 2 == 0)
      n <- 2
    else
      n <- 3
    newMat <- lapply(matSplit(mat, n), function(x){
        tempMat <- x
        ind <- 0
        for(rot in c(1:4)){
          index <- match(mat2patt(tempMat), rules)
          findex <- match(mat2patt(apply(tempMat, 2, rev)), rules)
          if(!is.na(index)){
            ind <- index
            break
          }
          else if(!is.na(findex)){
            ind <- findex
            break
          }
          tempMat <- matRot(tempMat)
        }
        out <- patt2mat(names(rules)[ind])
      }
    )
    patt <- mat2patt(matJoin(newMat))
  }
  # Count the pixels that are on.
  return(unname(table(strsplit(patt,""))["#"]))
}


main <- function(){
  
  start = Sys.time()
  
  # Reads the commandline argument
  argv <- commandArgs(TRUE)
  rawData <- readLines(argv[1])
  
  # Phase 1
  print(enhance(rawData, 5))
  # Phase 2
  print(enhance(rawData, 18))
  
  sprintf("Program took: %f", Sys.time()-start)
}

main()
