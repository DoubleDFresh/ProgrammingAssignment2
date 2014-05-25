## The two functions below (makeCacheMatrix and cacheSolve) work together to avoid repeatedly computing the 
## inverse of a matrix which is a costly computation.  
## It has 2 outcomes:
## 1. To cache the inverse of a matrix 'x' calculated below for re-use
## 2. To create the inverse of the matrix 'x'  

## makeCacheMatrix creates a matrix object that caches the inverse of the original matrix entered for 'x'.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The cacheSolve function returns a matrix that is the inverse of the original matrix 'x'.  It checks to 
## see if a cached value has already been computed.  
## If it does, it returns the cached value.  
## If it does not, it calculates the inverse of the matrix.

cacheSolve <- function(x, ...) {
         
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
