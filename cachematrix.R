## Programming Assingment 2 - R Programming
## This script contains pair of functions that cache the inverse of a matrix
## as follows:
## (1) makeCacheMatrix: Creates a special "matrix" object that can cache its 
##                      inverse.
## (2) cacheSolve: This function computes the inverse of the special "matrix" 
##              returned by makeCacheMatrix above.If the inverse has already been 
##              calculated, then cacheSolve retrieves the inverse from the cache.

## makeCacheMatrix: Starts here!
## This function takes object x which is a numeric matrix as argument
## And creates a vector that is a list containing the following functions:
##      (1) set : Set the value of a matix 
##      (2) get : get the value of a matix
##      (3) setinv : Set the value of a inverse matix
##      (4) getinv : get the value of a inverse matix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function() 
                x
        setinv <- function(inv) 
                m <<- inv
        getinv <- function() 
                m
        ##    return an object which can use those 4 internal functions
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
} ## makeCacheMatrix: Ends here!

## cacheSolve: Starts here!
## This function decides whether to provide cached inverse
## or to calculate the inverse
## Uses solve function to calculate inverse of a matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## Get matrix m from cache
        m <- x$getinv()
        ## If matrix m is not NULL return data from cache
        if(!is.null(m)){
                message("getting cached inverse matrix data")
                return(m)
        }
        ## If matrix m is NULL get original matrix
        data <- x$get()
        ## Calculate inverse using solve R function
        m <- solve(data, ...)
        x$setinv(m)
        ## Return inverse of matrix
        m
} ## cacheSolve: Ends here!
