## These functions compute the inverse of a matrix x, but first check
## whether the answer has been computed, and if so extracts it from the cache

## makeCacheMatrix creates a special 'matrix' which is used as a 
## cache for its inverse (actually a list of methods and a matrix property)

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


## cacheSolve uses the 'matrix' from above either to compute 
## the inverse or return the cached inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
        # Has the answer been cached?
        if(!is.null(m)) {
          message("getting cached data")
          return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        # Cache the result
        x$setinv(m)
}
