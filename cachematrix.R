## The following functions implement:
## - makeCacheMatrix: a class-like data structure storing a matrix 
##                    and its inverse.
## - cacheSolve: a function that, given an "instance" of a "cache 
##               matrix", returns its inverse, avoiding its computation
##               if it's already cached.

## Create a class-like instance storing a matrix and its associated
## inverse.
##
## Note that it's the user's responsibility to make sure the stored
## matrix and its inverse are in sync. No calculations/check are
## performed in the exposed functions.
##
## It exposes the following functions:
## - set(new_matrix): Sets the given matrix as the stored matrix.
## - get(): Returns the stored matrix.
## - setinverse(new_inverse): Set new_inverse to be the new inverse
##                            for the stored matrix.
## - getinverse(): Returns the stored inverse.
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(new_matrix) {
        x <<- new_matrix
        # New matrix stored: invalidate cached inverse (if any).
        cached_value <<- NULL
    }
    get <- function() x

    setinverse <- function(new_inverse) inverse <<- new_inverse
    getinverse <- function() inverse

    list(set = set, get = get,
         setinverse = setinverse, getinverse = getinverse)
}


## Return the inverse of the input cache matrix 'x'.
## If a cached value for the inverse is found in 'x', it'll
## be returned. Otherwise, the inverse will be calculated and
## 'x' updated with its value.
cacheSolve <- function(x, ...) {
    inverse <- x$getinverse()
    if (is.null(inverse)) {
        # The inverse wasn't cached: calculate it and store it in 
        # the cache.
        inverse <- solve(x$get(), ...) 
        x$setinverse(inverse)
    } else {
        # Quick and dirty way of checking whether we are using the
        # cached inverse.
        message("Cache hit!")
    }
    inverse
}
