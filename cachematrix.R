## This file is to satisfy Data Science Coursera Programming Assignment 2
## The functions will cache the inverse of a matrix

## makeCacheMatrix creates a function that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setinv <- function(solve) m <<- solve
      getinv <- function() m
      list(set = set, get = get,
           setinv = setinv,
           getinv = getinv)

}


## cacheSolve computes the inverse of a matrix returned by makeCacheMarix.
## if the inverse has already been calculate, cacheSolve will retrieve it
## from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      m <- x$getinv()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setinv(m)
      m
}
