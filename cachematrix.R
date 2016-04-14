## Pair of functions that can calculate and cache the inverse of a matrix using the <<- operator

## This function creates a special matrix object (list) that can cache its inverse.
## Creates function that where the default is x is matrix - function creates a list of 4 functions
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function (y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) i <<- inv
  getinverse <- function() i
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## This function computes the inverse of the special matrix returned by makeCacheMatrix.
## If already calculated and the matrix is the same, cacheSolve returns the inverse from cache.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data,...)
  x$setinverse(i)
  i
}
