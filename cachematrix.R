# By the following functions the inverse of a matrix can be retrieved from cache
# instead of making numerous calculations  The input square matrix must be
# nonsingular.

## The MakeCacheMatrix function creates a special "matrix" object that can cache
## its inverse

makeCacheMatrix <- function(x = matrix()) {
    
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}
##  The function cacheSolve computes the inverse of the special "matrix"
##  returned by makeCacheMatrix above. If the inverse has already been
##  calculated (and the matrix has not changed), then the cachesolve should
##  retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    m <- x$getsolve()
    if(!is.null(m)) {
        print("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m    
}
