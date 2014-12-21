# Coursera - R Programming - Assignment 2
# Following 2 functions cache the inverse of a matrix.
# Assumption: The matrix supplied is always invertible.
#
# 1. makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
# receives the input matrix in matrix1 param. 
# it has functions to set and get the value of the matrix, and set and get the value of the inverse 
# Uses solve function to get the inverse. 
# Cached value of the inverse is available in inverse1 variable. 
makeCacheMatrix <- function(matrix1 = numeric()) {
  
  inverse1 <- NULL          	
  
  set <- function(matrixValue) {
    matrix1 <<- matrixValue
    inverse1 <<- NULL
  }
  
  get <- function() { 
    matrix1 
  }
  
  setInverse <- function (solve) {
    inverse1 <<- solve
  }
  
  getInverse <- function() {
    inverse1
  }
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


# cacheSolve: This function computes the inverse of the matrix returned by makeCacheMatrix. 
# If the inverse has already been calculated, and the matrix has not changed, 
# then the cacheSolve retrieves the inverse from the cache.
cacheSolve <- function(matrix2, ...) {
  
  inverse2 <- matrix2$getInverse()
  if(!is.null(inverse2)) {
    message("getting cached data")
    return(inverse2)
  }
  
  data <- matrix2$get()
  inverse2 <- solve(data, ...)
  matrix2$setInverse(inverse2)
  inverse2
}