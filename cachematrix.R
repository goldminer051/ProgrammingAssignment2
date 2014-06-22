## Programming Assingment 2 - R Programming
## This script contains pair of functions that cache the inverse of a matrix
## as follows:
## (1) makeCacheMatrix: Creates a special "matrix" object that can cache its 
## inverse.
## (2) cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above.If the inverse has already been 
## calculated, then cacheSolve retrieves the inverse from the cache.

## makeCacheMatrix: Starts here!

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function() 
                x
        setinv <- function(solve) 
                m <<- solve
        getinv <- function() 
                m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
} ## makeCacheMatrix: Ends here!

## cacheSolve: Starts here!

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
        if(!is.null(m)){
                message("getting cached matrix data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
} ## cacheSolve: Ends here!
