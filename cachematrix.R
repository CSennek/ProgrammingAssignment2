## Create cacheable matrix inverse
## Returns a list of functions to get and set the value of said functions
## Input matrix must be square and invertible

makeCacheMatrix <- function(x = matrix()) {
  ##initiate object
  inv <- NULL
  
  ##sets a matrix
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  
  ##gets the matrix
  get <- function() x
  ##sets the inverse of the matrix
  setinverse <- function(x) inv <<- solve(x)
  ##gets the inverse of the matrix
  getinverse <- function() inv
  
  ##returns a list of the prior functions
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Inverts the matrix from makeCacheMatrix
## if already cached, gets inverted matrix from cache

cacheSolve <- function(x, ...) {

  ##set inv to the inverse of 'x'
  inv <- x$getinverse()
  ##get cahced matrix if one exists
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  ##get makeCacheMatrix 
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  
  ##returns inverse of 'x'
  inv
}
