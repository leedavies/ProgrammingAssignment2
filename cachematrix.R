## Caches the potentially costly computation of the inverse of a matrix.
## makeCacheMatrix creates a list that can cache the inverse of the matrix.
## cacheSolve computes the inverse of the special 'matrix' returned by makeCacheMatrix. 
## If the inverse has already been calculated the cache value will be returned.

## The function creates a list that stores the matrix parameter and is used
## to cache the inverse of the matrix.
## Parameters:  x the matrix to cache, default = matrix()
## Return:      list(set, get, getsolve, setsolve)
##                  get() returns the matrix from the cache
##                  set() sets the metrix within list in the cache
##                  getsolve() returns the inverse of the matrix from the cache
##                  setsolve() sets the solve value in the cache for the matrix

makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) s <<- solve
    getsolve <- function() s
    list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## Calculates the inverse of the special 'matrix' returned by makeCacheMatrix and caches the
## result. If the inverse of the matrix has previously been calculated the method returns the
## inverse matrix from the cache.
## Parameters:  x the special 'matrix' returned from makeCacheMatrix.
##              ... parameters for inverse calculation
## Retun:       matrix (calculated or returned from cache)

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    s <- x$getsolve()
    if(!is.null(s)) {
        message("getting cached data")
        return(s)
    }
    data <- x$get()
    s <- solve(data, ...)
    x$setsolve(s)
    s
}
