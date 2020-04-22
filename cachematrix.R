## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix: This function " makeCacheMatrix" creates a special "matrix" object that can cache its own inverse.

makeCacheMatrix <- function(x = matrix()) {
  mi <- NULL
  set <- function (y){
        x <<- y
        mi <<- NULL
  }
      get <- function() x
      setinverse <- function(inverse) mi<<- inverse
      getinverse <- function() mi
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


## The following function calculates the inverse of the special Matrix created with the above function. 
## However, it first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. Otherwise, it calculates the inverse of the matrix and sets the value of the inverse matrix in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
    mi <- x$getinverse()
    if(!is.null(mi)) {
      message("getting cached data")
      return(mi)
    }
    data <- x$get()
    mi <- solve(data, ...)
    x$setinverse(mi)
    mi
}
