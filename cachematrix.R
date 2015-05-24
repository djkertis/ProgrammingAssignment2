## This module contains two functions to mange the inverse 
#    calcuation of a matrix.
## makeCacheMatrix - Returns a list of functions to store the
##    matrix and its cached inverse.
## 
## cacheSolve - Will get the inverse of the matrix using
##    the cached value if it exists.

## Write a short comment describing this function
## This function will create a list to store a list 
##   of cached inverses.
## x - a matrix to lookup the cached inverse of
## Returns a list of function to manage the caching.
makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}



## Return a matrix that is the inverse of 'x'
## Will return a cached value if it exists.
cacheSolve <- function(x, ...) {
	m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- inverse(data, ...)
        x$setinverse(m)
        m
}
