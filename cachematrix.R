## Put comments here that give an overall description of what your
## functions do

## This function creates a special "cached matrix" object
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    mat <- x
    set <- function(y) {
        mat <<- y
        inverse <<- NULL
    }
    get <- function() mat
    getInverse <- function() inverse
    setInverse <- function(i) inverse <<- i
    list (set = set, get = get, getInverse = getInverse,
          setInverse = setInverse)
}


## This function computes the inverse of a "cached" matrix.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse <- x$getInverse()
    if ( !is.null(inverse) ) {
        message("getting the inverse matrix from the cached object")
    } else {
        message("computing the inverse and caching it")
        inverse <- solve(x$get(), ...)
        x$setInverse(inverse)
    }
    inverse
}
