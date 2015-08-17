## The following functions will inverse a matrix and store
## the results in cache

## Function that creates a special matrix object that can
## cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    ## Initialize m
    m <- NULL
    
    ## Set working evnironment variables
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    ## Get matrix
    get <- function() x
    ## Set inverse function
    setInv <- function(inverse) m <<- inverse
    ## Get inverse function
    getInv <- function() m
    ## Return list
    list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## Computes the inverse of the special "matrix" returned above

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getInv()
    
    ## Check to see if matrix inversion is already stored in cache
    if(!is.null(m)) {
        message("getting cached data")
        ## if so return the cache matrix
        return(m)
    }
    
    ## Calculate the inversion of the matrix
    dat <- x$get()
    m <- solve(dat, ...)
    
    ## Store inverted matrix in cache 
    x$setInv(m)
    
    ## Return inverted matrix
    return(m)
}
