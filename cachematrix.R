# The functions below caches the value of a matrix and its inverse.
# 
# The function makeCacheMatrix takes for its argument a matrix and returns a
# four-element list:
#   $set is a function that allows to set the value of the cached matrix
#   $get is a function that returns the value of the cached matrix
#   $setinv is a function that calculates and caches the inverse
#   $getinv is a function that returns the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(solve) m <<- solve
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

# cacheSolve is a function that inverts a matrix as cached by
# makeCacheMatrix.  If the inverse has calculated, it retrieves
# the cached version, if it has not it inverts.
# cacheSolve takes for an argument a list from makeCacheMatrix.

cacheSolve <- function(x, ...) {
    m <- x$getinv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
}
