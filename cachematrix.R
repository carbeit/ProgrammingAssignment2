## Inverts the matrix, and creates a cache of the inverted matrix.
## This method uses an object ("makeCacheMatrix") acting as a matrix.
## This object implements the cache
## When a value exists within the cache the existing value is cleared (reset
## with a value of null) if object is modified (through set()).


makeCacheMatrix <- function(m = matrix()) {
    store <- NULL
    set <- function(m2) {
        m <<- m2
        store <<- NULL
    }
    get <- function() m
    setinverse <- function(inverse) store <<- inverse
    getinverse <- function() store
    list(set = set,
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}



## cacheSolve prints the inverse of the matrix makeCacheMatrix.
## If an inverse is already calculated, it is printed
##  Otherwise, it calculates the new inverse.

cacheSolve <- function(m, ...) {
    result <- m$getinverse() #See above for getinverse()
    if(!is.null(result)) {
        message("cached result")
    } else {
        matrixdata <- m$get()
        result <- solve(matrixdata, ...)
        m$setinverse(result) #writes the solution to m (above)
    }
    result
}
