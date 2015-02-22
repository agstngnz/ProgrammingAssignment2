## cacheSolve computes the inverse of the special "matrix" returned by 
## makeCacheMatrix. If the inverse has already been calculated (and the matrix 
## has not changed), then the cachesolve retrieves the inverse from the cache.

## This function creates a special "matrix" object that caches the inverse matrix 
## computed by cacheSolve below. 
## This object is a list made up of functions: set, get, setinverse, and getinverse.
## Functions set() and setinverse() assign the matrix passed as an argument to their 
## corresponding named list member 'set' and 'setinverse'.
## Functions get() and getinverse() retrieve their corresponding assigned matrices. 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(a1) m <<- a1
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix and assigns the calculated inverse matrix into setinverse
## If the inverse has already been calculated (and the matrix 
## has not changed), then the cacheSolve retrieves the inverse from the cache.


cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
