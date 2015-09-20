## makeCacheMatrix: 
## This function creates a special "matrix" object that can cache its inverse.
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
    # Return a list of 'objects'
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## cacheSolve
## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        ## Inverse is already calculated.
        message("getting cached data")
        return(inv)
    }
    ## Inverse isn't calculated yes, We do
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}
