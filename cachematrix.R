## Coursera Week3 Programming assignment:
## Caching the Inverse of a Matrix - Matrix inversion is usually a costly
## computation and there may be some benefit to caching the inverse of a matrix
## rather than compute it repeatedly. This program contains a pair of
## of functions that cache the inverse of a matrix.

## ---------------------------------------------------------
## Example of an intended use:
## > ex <-makeCacheMatrix(matrix(c(-2.5, 1.5, 2, -1),ncol=2,nrow=2))
## > cacheSolve(ex)
##       [,1] [,2]
##  [1,]    2    4
##  [2,]    3    5
## > cacheSolve(ex)
## getting cached data
##       [,1] [,2]
##  [1,]    2    4
##  [2,]    3    5
## ---------------------------------------------------------

## makeCacheMatrix: This function creates a special "matrix" object that can
## cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(solve) inv <<- solve
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## cacheSolve: This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then the cachesolve should return its inverse

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
