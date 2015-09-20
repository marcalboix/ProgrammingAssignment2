## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# This function 
makeCacheMatrix <- function(x = matrix()) {
    # Init: NULL value for inverse matrix if cacheSolve has not used.
    inv <- NULL 
    # Assignement of x (matrix variable) if set() is used.
    set <- function(y) { 
        x <<- y
        inv <<- NULL
    }
    # Return matrix variable
    get <- function() x 
    # Assignement of inverse matrix variable.
    setinverse <- function(invmatrix) inv <<- invmatrix 
    # Return inverse matrix variable
    getinverse <- function() inv #
    # Return a list
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Write a short comment describing this function
## Purpose: calculate the inverse of a matrix (special makeCacheMatrix) and put 
## the result in the inverse variable.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}

a = matrix(c(1,2,4,3,4,5,-4,3,1), nrow=3, byrow=TRUE)
cm = makeCacheMatrix(a)
cm$set()
cm$get()
cm$getinverse()
cacheSolve(cm)
cm$getinverse()
