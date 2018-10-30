## Pair of functions to cache the inverse of a matrix.

## This function makes an artificial matrix by creating a list which contains a set
## of functions to modify the list's contents. These functions include 
## one to set the value of a matrix, other to retrieve it, one to set its inverse
## and a last one to get its inverse. setinverse() is designed to work only when called
## by the function cacheSolve(). getinverse() only works after the inverse has been 
## stored by cacheSolve().
makeCacheMatrix <- function(x = matrix()) {
    z <- NULL
    set <- function(y) {
        x <<- y
        z <<- NULL
    }
    get <- function() x
    setinverse <- function(inv) z <<- inv
    getinverse <- function() z
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}
## This function retrieves the matrix's inverse in case it has been already stored
## in the original list. If the inverse has not been calculated, this function calls 
## the matrix stored by the first function, then calculates its inverse and finally
## use the function setinverse() to store its value in the original list.
cacheSolve <- function(x, ...) {
    z <- x$getinverse()
    if (!is.null(z)) {
        message("getting cached matrix")
        return(z)
    }
    data <- x$get()
    z <- solve(data, ...)
    x$setinverse(z)
    z
}

