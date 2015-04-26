## These functions take the inverse of a matrix and caches the 
## result.

## makeCacheMatrix is a function which stores a list of four
## functions: set, get, setinverse, and getinverse. 
## set changes the matrix stored in the function.
## get returns the matrix stored in the function.
## setinverse stores the value of the inverse.
## getinverse returns the value of the inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
      x <<- y
      i <<- NULL
  }
  get <- function() x 
  setinverse <- function(solve) m <<- solve
  getinverse <- function () m
  list(set = set, get = get, setinverse = setinverse, 
       getinverse = getinverse)
}


## cacheSolve computes the inverse of the matrix 
## from makeCacheMatrix. It checks that the "getinverse" value
## is not null and also if it has already been stored in memory. If
## it has been stored, it returns the value. If not, it calculates
## the inverse.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
      message("Getting cached data")
      return (m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}