## In this assigment I have tried to write a pair of functions
## that cache the inverse of a matrix.

## makeCacheMatrix contains four functions: set, get, 
## setInv and getInv.
## Set and get are used to establish, change or obtain the 
## matrix we want to invert.
## SetInv and getInv are used to cache de inverse of the
## chosen matrix and recover it without recomputing.

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setinv <- function(inv) m <<- inv
      getinv <- function() m
      list(set = set, get = get,
           setinv = setinv,
           getinv = getinv)
}

## cacheSolve calculates the inverse of the matrix created  
## with makeCacheMatrix. If it has already been obtained 
## (!is.null(m)=TRUE), the function return the precomputed
## values.

cacheSolve <- function(x, ...) {
      m <- x$getinv()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- solve(data)
      x$setinv(m)
      m
}
