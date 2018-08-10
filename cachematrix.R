## These functions, when combined, convert a matrix into the 'cacheMatrix' (the list) and then
## solve for the inverse using the solve function. 
## This is very similar to the example, because I used the example as a template.

## Takes in a matrix and converts it to the cache matrix
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  get <- function() x
  setInverse <- function(Inverse) i <<- Inverse
  getInverse <- function() i
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}
## Takes in the cache matrix object and solves for the inverse of the original matrix. Returns
## the inverted matrix
cacheSolve <- function(x, ...) {
  i <- x$getInverse()
  
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  data <- x$get()
  ##Uses the solve function to get the inverse of the matrix
  i <- solve(data, ...)
  x$setInverse(i)
  i
  ## Returns the matrix that is the inverse of 'x' (the cached original matrix)
}
