## Two functions which a) create a "special" matrix whose inverse
## can be calculated then cached, b) calculates the inverse of the
## "special" matrix or uses the cached value if already calculated.

## Creates a "special" matrix whose inverse can be cached after
# calculation to avoid the need to recalculate it in future.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Finds inverse of "special" matrix created by makeCacheMatrix.
# If the inverse has already been calculated,
# use that cached inverse rather than recalculating it.

cachesolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}


