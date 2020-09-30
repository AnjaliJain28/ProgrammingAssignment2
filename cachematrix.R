## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  #default value of cache
  mat <- NULL
  
  # Set the value of the matrix.
  setMatrix <- function(matrix_value) {
    x <<- matrix_value
    # since the matrix is assigned a new value, flush the cache
    mat <<- NULL
  }
  
  # Get the value of the matrix.
  getMatrix <- function() {
    x
  }
  
  # Set the inverse function value of the matrix.
  setInverse <- function(solve) {
    mat <<- solve
  }
  
  # Get the inverse function value of the matrix.
  getInverse <- function() {
    mat
  }
  
  # Return the matrix values stored in a list.
  list(setMatrix = setMatrix, getMatrix = getMatrix, 
       setInverse = setInverse, getInverse = getInverse)
}

## Write a short comment describing this function

## Cahe Matrix Solve Function
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  # # Get inverse matrix cache value in matrix variable.
  inverse <- x$getInverse()
  
  # if matrix != 0 then return cache inverse matrix
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  
  #get the matrix value and stored in inverse variable
  data <- x$getMatrix()
  inverse <- solve(data)
  x$setInverse(inverse)
  
  #return inverse of cache matrix
  inverse
}

## test the output of the functions.
##matrix <- makeCacheMatrix( matrix(c(5,6,8,20,9,10,13,44,36), nrow = 3, ncol = 3) )
##summary(matrix)
##matrix$getMatrix()
##cacheSolve(matrix)

