## These functions implement cached matrix inversion

## Returns a list with functions for caching a matrix and its inverse
# set(y) stores the matrix y.
# get(y) returns the matrix y.
# setinv(inv) stores the value inv in the inverse of the matrix.
# getinv(inv) returns the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Calculates the inverse and stores it in the cache. If the inverse already exists, then
## it pulls it from the cache instead.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
