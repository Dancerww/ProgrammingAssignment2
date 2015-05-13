## This function cache the inverse of a given matrix, which avoids
## repetitive computational-expensive 'solve' functions.

## Create a matrix object containing a list of four functions,
## namely to retrieve and assign the values of the matrix and its
## inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## Retrieve the inverse of a matrix from cache, or compute and
## cache one if none exists. Note: the message("getting cached
## data") for debugging purpose has been removed from the 'if'
## clause as in the example.

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) return(inv)
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
