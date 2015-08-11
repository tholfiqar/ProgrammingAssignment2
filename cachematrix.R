# Rather than computing the inverse of a matrix repeatedly which
# is usually time-consuming operation we used the following functions
# to cache the inverse of a matrix.


# The following function creates a list containing functions to
# set the value of matrix x
# get the value of matrix x
# set the value of the inverse of matrix x
# get the value of the inverse of matrix x

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(par) inv <<- par
    getinverse <- function() inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)    
    
    
}


# The following function returns the inverse of the matrix x 
# It checks if the inverse has already been computed
# If it has been computed
#   it gets the result and skips the computation. 
# If not
#   it computes the inverse, sets the value in the cache via setinverse function.

cacheSolve <- function(x, ...) {
    
    inv <- x$getinverse()
    
    # check if the inverse has already been computed. 
    if(!is.null(inv)) {
        
        # notify user
        message("getting cached data.")
        
        # get the result and skip the computation.
        return(inv)
        
    }
    data <- x$get()
    
    # compute the inverse
    inv <- solve(data)
    
    # set the value in the cache
    x$setinverse(inv)
    
    # return the inverse
    inv
}
