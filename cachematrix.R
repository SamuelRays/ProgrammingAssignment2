## Two functions down there are to prevent the extra
## computations to be held where they can be avoided.
## makeCacheMatrix function creates a "matrix storage" that
## contains an invesed matrix if it's computed already.
## cacheSolve function takes the "matrix storage"
## created by makeCacheMatrix function as the argument and
## retuns the inversed matrix either got from the "matrix storage"
## or computed if it's never been computed before.

## This function returns the list of methods that provide
## the access to both original and inversed matrices.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    get <- function() x
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    getInv <- function() inv
    setInv <- function(i) {
        inv <<- i
    }
    list(get = get, set = set, getInv = getInv, setInv = setInv)
}


## This function returns the inversed matrix.
## Either cached or computed.

cacheSolve <- function(x, ...) {
    inv <- x$getInv()
    if(is.null(inv)) {
        data <- x$get()
        message("computing...:(")
        inv <- solve(data, ...)
        x$setInv(inv)
    }
    inv
}
