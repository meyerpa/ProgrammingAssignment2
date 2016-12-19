## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        get <- function() x
        setCacheMatrix <- function(cacheMatrix) m <<- cacheMatrix
        getCacheMatrix <- function() m
        list(set = set, get = get, setCacheMatrix = setCacheMatrix,
             getCacheMatrix = getCacheMatrix
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getCacheMatrix()
        if (!is.null(m)) {
                message("getting cached data")
                return m
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setCacheMatrix(m)
        m
}
