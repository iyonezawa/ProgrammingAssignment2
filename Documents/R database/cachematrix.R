## Overall, makeCacheMatrix and cacheSolve together will return an inverse of 
## matrix effectively in a way that we can avoid calculating it more than once 
## for a matrix we input as an argument. 

# makeCacheMatrix produces functions of getting and setting the values of matrix 
# and inverse of the matrix and creates the list of these functions.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
set <- function(y) {
    x <<- y
    i <<- NULL}
get <- function() x
setinverse <- function(solve) i <<- solve
getinverse <- function() i
list(set = set, get = get,
     setinverse = setinverse,
     getinverse = getinverse)
}


# cacheSolve returns the inverse of the matrix. 
# If a cached data is available, this will return the cached data.

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
