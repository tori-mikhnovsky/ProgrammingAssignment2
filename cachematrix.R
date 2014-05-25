## makeCacheMatrix creates a list containing a function to
#1 set the Matrix
#2 get the Matrix
#3 set the Inverse of a matrix
#4 get the Inverse of a matrix


makeCacheMatrix <- function(x = matrix()) {
        mInverse <- NULL
        set <- function(y) {
                x <<- y
                mInverse <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) mInverse <<- inverse
        getInverse <- function() mInverse
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

## cacheSolve calculates the Inverse of the matrix
##it first checks to see if the Inverse has already been calculated and if so it gets the Inverse from the cache
##otherwise, it calculates the Inverse and stores it in cache


cacheSolve <- function(x, ...) {
        m <- x$getInverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        matrix <- x$get()
        m <- solve(matrix, ...)
        x$setInverse(m)
        m
}
