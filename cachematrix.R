# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly. The
# following two functions are used to cache the inverse of a matrix.

# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

## Note this is not all my original work as I am not that good at programming!!!
makeCacheMatrix <- function(x = matrix()) {
## This is the controller and first make inv NULL     
    inv <- NULL
## Set values for out of context check using function
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
## Function to set and get    
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


# The following function returns the inverse of the matrix. It first checks if
# the inverse has already been computed. If so, it gets the result and skips the
# computation. If not, it computes the inverse, sets the value in the cache via
# setinverse function.

# This function assumes that the matrix is always invertible.
cacheSolve <- function(x, ...) {
## inv is set to value of x$getinverse funtion and checking if not set.      
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data.")
        return(inv)
    }
## If found it is returned else it recalculates as below   
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}