## Assignment: Caching the Inverse of a Matrix.
## Matrix inversion is usually a costly computation and there may be some benefit to caching 
## the inverse of a matrix rather than compute it repeatedly. 
## The assignment is to write a pair of functions that cache the inverse of a matrix.

## The function makeCacheMatrix creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInv <- function(inverse) m <<- inverse
        getInv <- function() m
        list(set = set, get = get, 
             setInv = setInv, 
             getInv = getInv)
}


## The function cacheSolve will compute the inverse of the "special" matrix created by 
## the makeCacheMatrix function detailed above.
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cacheSolve function will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInv(m)
        m        
}
