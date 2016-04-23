## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix caches the matrix and its inverse(when asked)
## The return value is the list object with functions to get/set matrix and inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    
    setsolve <- function(sol) inv <<- sol
    getsolve <- function() inv
    
    list(set=set, get=get, setsolve = setsolve, getsolve=getsolve)
}


## cacheSolve function calcualtes and set the inverse into cache if it is not available

cacheSolve <- function(x, ...) {
        s <- x$getsolve()
        if(!is.null(s)) {
            message("getting cached inverse matrix")
            return(s)
        }
        mat <- x$get()
        s <- solve(mat, ...)
        x$setsolve(s)
}
