## This function cache the inverse of a given matrix, which avoids
## repetitive computational-expensive 'solve' functions.

## Create a matrix object containing a list of four functions,
## namely to retrieve and assign the values of the matrix and its
## inverse.
##
##  set()     to set the value of matrix, and clear cached inverse
##  get()     to retrieve the value of stored matrix
##  setinv()  to cache the value of the inverse
##  getinv()  to retrieve the value of stored inverse

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
## cache one if none exists. 

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
