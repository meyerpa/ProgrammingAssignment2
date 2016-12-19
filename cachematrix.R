## makeMatrix caches a matrix, and uses cacheSolve to solve the inverse matrix. This is useful if 
## the matrix inverse is never used (ignoring the long computation to compute the matrix inverse)
## or the matrix inverse is used multiple times (thus only needing to solve the matrix once

## makeMatrix creates a special "matrix", which is really a list containing a function to
## set the cached matrix
## get the matrix
## set the inverse of the matrix
## get the inverse of the matrix
makeMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        get <- function() x        
        setInverse <- function(matrix) m <<- matrix
        getInverse <- function() m
        list(set = set, get = get, setMatrix = setMatrix,
             getMatrix = getMatrix)
}


## cacheSolve first checks to see if the matrix inverse has already been calculated. 
## If so, it gets the inverse matrix from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the matrix and sets the value of the 
## inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        if (!is.null(m)) {
                message("getting cached data")
                return m
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        m
}
