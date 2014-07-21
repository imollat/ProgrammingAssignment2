## The following functions will allow the creation of a cachable matrix object with the
## ability to cache its inverse. It can be used for optimization purposes in which the
## inverse is calculated only once for an unchanged matrix.

## The following function expects a square numerical matrix as input and will create
## a new object that not only caches the original matrix, but also its inverse, once
## calculated and set.
makeCacheMatrix <- function(x = matrix()) {

    # check if input x is not valid
    if(!validEntry(x)) { return(NULL) }
        
    im <- NULL
    set <- function(y) {
        if(validEntry(y)) { 
            x <<- y
            im <<- NULL
        }
    }
    get <- function() x
    setinverse <- function(inverse) im <<- inverse
    getinverse <- function() im
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## The following function does some simple checks on whether x is a square inversable matrix
validEntry <- function(x) {
    # check if x not a matrix...
    if (!is.matrix(x)) {
        message("not a matrix")
        return(FALSE)
    }
    # ...and whether it is not square...
    else if (nrow(x) != ncol(x)) {
        message("matrix not square")
        return(FALSE)
    } # ...and determinant equal to zero (not inversable)
    else if (det(x) == 0) {
        message("matrix is singular")
        return(FALSE)
    }    
    
    return(TRUE)    
}

## The following function will take a cacheable matrix object (as created via the
## makeCacheMatrix function) and return its inverse. Depending on whether it has been done
## before in this context, the inverse will be either taken from cache or calculated and then
## stored in cache.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    im <- x$getinverse()
    if(!is.null(im)) {
        message("getting cached data")
        return(im)
    }
    data <- x$get()
    im <- solve(data, ...)
    x$setinverse(im)
    im
}
