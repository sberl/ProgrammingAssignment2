## This is a special version of a matrix data structure, which is able to
## cache the value of the inverse of a matrix in order to improve performance
## of programs which need to solve for the inverse of a matrix multiple
## times.

## Use this function to initialize the matrix.

makeCacheMatrix <- function(x = matrix()) {
    cachedInv <- NULL
    set <- function(y) {
        x <<- y
        cachedInv <<- NULL
    }
    get <- function() x
    setinverse <- function(inv) cachedInv <<- inv
    getinverse <- function() cachedInv
    
    list(set = set, 
         get = get, 
         setinverse = setinverse, 
         getinverse = getinverse)
}


## This function will return the inverse of the matrix. It will either
## retrieve it from the cache, or solve it.

cacheSolve <- function(x, ...) {
    ## Check for a cached value
    cachedval <- x$getinverse()
    if (!is.null(cachedval)) {
        # There was a cached value, and the matrix has not changed?
        message("getting cached data")
        cachedval # return cached inverse
    } else {
        # no cached value
        # Need to recalculate
        data <- x$get()
        inv <- solve(data)
        
        # Save solution in case they ask again
        x$setinverse(inv)
        inv  # return inverse
    }
}
