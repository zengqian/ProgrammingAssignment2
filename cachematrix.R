
#This program creates a special object that stores a matrix and cache's its inverse

## This function creates a list of functions to
#1) set the matrix
#2) get the matrix
#3) set the inverse
#4) get the inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinv <- function(i) inv <<- i
    getinv <- function() inv
    list(set = set, get = get,
    setinv = setinv,
    getinv = getinv)
}


### This function calculate the reverse matrix to a given matrix.
#However, it first checks to see if the reverse has already been calculated. If so, it gets the reverse from the cache and skips the computation.
#Otherwise, it calculates the reverse of the matrix and sets the inverse of the matrix in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinv()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinv(i)
    i
}
