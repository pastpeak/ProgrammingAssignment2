## Computing the inverse of a matrix can be a time-consuming operation.
## The functions below let the user create a special "matrix" object
## that caches its inverse so that subsequent calls to compute the 
## inverse return the cached value instead of recomputing it.

## The following function creates a "matrix" object that caches its inverse. 
## What is actually created is a list of four functions that:
##  1. set the value of the matrix
##  2. get the value of the matrix
##  3. set the value of the inverse
##  4. get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
    invrs <- NULL
    set <- function(y) {
        x <<- y
        invrs <<- NULL
    }
    get <- function() x
    setinverse <- function(i) invrs <<- i
    getinverse <- function() invrs
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## The following function calculates the inverse of the special "matrix" created with the 
## above function. However, it first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. Otherwise, it calculates 
## the inverse of the data and sets the value of the inverse in the cache via the setinverse function.
cacheSolve <- function(x, ...) {
    # Get the cached value for the inverse, if any
    invrs <- x$getinverse()
    # See if the inverse has already been cached
    if (!is.null(invrs)) {
        # The inverse is cached, so return the cached value
        message("getting cached data")
        return(invrs)
    }
    # Inverse not cached, so we need to compute it
    # Get the data for the matrix
    data <- x$get()
    # Use the solve function to compute the inverse
    invrs <- solve(data, ...)
    # Cache the inverse
    x$setinverse(invrs)
    # Return the inverse
    invrs    
}
