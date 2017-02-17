## this is my assignment for peer reviewing including 2 functions : the first creates a matrix, which can cache its inverse and the second can retrieve the inverse when already computed or compute the inverse of the matrix returned in the first function

## first function makeCacgeMatrix creates a special "matrix" object
## that can cache its inverse.

makeCacheMatrix<- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## second function cacheSolve computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above.

cacheSolve <- function(x, ...) {
              m <- x$getinverse()
              if(!is.null(m)) {
                     message("getting cached data")
                     return(m)
                     }
                data <- x$get()
                 m <- solve(data, ...)
                x$setinverse(m)
                m
}
