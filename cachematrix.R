####################################################################################################################
##  
## author: kudduesi uenlue
## date:   09.11.2016
##
## 1.) makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
##
## 2.) cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##     If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should 
##     retrieve the inverse from the cache.
##
## assumption: the matrix supplied is always invertible.
##
####################################################################################################################

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix  <- function( myMatrix = matrix() ){
  
  myInverse <- NULL
  
  setMatrix <- function(argMatrix) {
    myMatrix  <<- argMatrix
    myInverse <<- NULL
  }
  
  getMatrix  <- function() myMatrix
  
  setInverse <- function(inverse) myInverse <<- inverse
  
  getInverse <- function() myInverse
  
  list(setMatrix  = setMatrix, 
       getMatrix  = getMatrix,
       setInverse = setInverse,
       getInverse = getInverse)
  
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.

cacheSolve <- function( oMatrix, ... ){
  
  inverse <- oMatrix$getInverse()
  
  if(!is.null(inverse)) {
    message("!!! inverse of this matrix was already calculated !!!")
    return(inverse)
  }
  
  calcMatrix <- oMatrix$getMatrix()
  inverse <- solve( calcMatrix , ... )
  oMatrix$setInverse( inverse )
  inverse
  
}
