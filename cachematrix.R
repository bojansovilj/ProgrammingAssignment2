## function makeCacheMatrix creates special matrix which is list containing the functions for:
##   1. set the value of a matrix
##   2. get the value of a matrix
##   3. set the value of invert matrix
##   4. get the value of invert matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinvert <- function(invert) inv <<- invert
    getinvert <- function() inv
    list(set = set, get = get,
         setinvert = setinvert,
         getinvert = getinvert)
    
}


## function cacheSolve calculates the invert of the "matrix" created in makeCacheMatrix
## First we check if invert is already been calculated. If yes we skip calculation and return cached value,
## if not we calculate invert and sets its value.

cacheSolve <- function(x, ...) {
    inv <- x$getinvert()
    if (!is.null(inv)) {
        message("getting cached data...")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinvert(inv)
    inv
}
