## Put comments here that give an overall description of what your
## functions do

## The method 'makeCacheMatrix' creates a different kind of "matrix" containing a function to:
## set the value of the matrix
## get the value of the matrix
## set the value of the inverted matrix
## get the value of the inverted matrix

## Parameters:
## 'x' is an input squared matrix (invertible)
## Returns a list of declared funcions: $set, $get, $setsolve, $getsolve

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setsolve <- function(inv) i <<- inv
        getsolve <- function() i
        list(set = set, get = get,
             setsolve =  setsolve,
             getsolve = getsolve)
}

## The method 'cacheSolve' calculates the mean of the customized "matrix" created with the above function. 
## However, it first checks to see if the solve operation has already been calculated. 
## If so, it gets the result from the cache through the method 'getsolve' and skips the computation. 
## Otherwise, it performs the calculation of the inverted matrix and sets the value in the cache via the 'setsolve' function.

## Parameters:
## 'x' is a 'makeCacheMatrix' object (customized matrix)
## Returns a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getsolve()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setsolve(inv)
        inv
}
