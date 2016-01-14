## With the functions MakeCacheMatrix and cacheSolve the inverse of matrices 
## can be stored in cache, so that time can be saved for computing.
##
## MakeCacheMatrix creates a special matrix object, in which the inverse 
## of the matrix can be cached.
##

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(solve) inv <<- solve
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

## cacheSolve checks if the inverse is cached already (returned by MakeCacheMatrix) . 
## If so, the function retrieves the inverse from cache. 
## If not, the inverse is calculated.

cacheSolve <- function(x, ...) {
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
