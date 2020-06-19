# A pair of functions that can compute the inverse of a matrix

# Creating a special matrix that can cache its inverse. 

makeCacheMatrix <- function(x = matrix()) {
  # The inverse
  i <- NULL
  
  # Setting the matrix
  setmatrix <- function(matrix) {
    x <<- matrix
    i <<- NULL
  }
  # Getting the matrix
  getmatrix <- function() {
    x # Returning the matrix
  }
  # Setting the inverse to the matrix
  setinverse <- function(inverse) {
    i <<- inverse
  }
  # Getting the inverse of the matrix
  getinverse <- function() {
    i # Returning the inverse
  }
  list(setmatrix = setmatrix, getmatrix = getmatrix, 
       setinverse = setinverse, getinverse = getinverse)
}


# Computes the inverse of the special matrix returned by "makeCacheMatrix" above.
# If the matrix has already been calculated, "cachesolve" will retrieve the 
# inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  
  if (!is.null(x)){
    message("getting cached data")
    return(m)
  }
  
  # Getting the matrix.
  data <- x$getmatrix()
  
  # Calculating inverse using matrix multiplication.
  m <- solve(data) %*%  data
  
  # Setting the inverse
  x$setinverse(m)
  
  # Returning the matrix
  m
}
