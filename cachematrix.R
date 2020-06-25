## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  # Initialize inv to null
  inv <- NULL
  
  # New value of matrix is assigned to parent, ow null
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # Returns matrix value
  get <- function() x
  
  # Assigns value of inv in parent env
  setinverse <- function(inverse) inv <<- inverse
  
  # Gets value of inverse
  getinverse <- function() inv
  
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function
# Solution 
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  data <- x$get()
  
  inv <- solve(data, ...)
  
  x$setinverse(inv)
  
  inv
}
