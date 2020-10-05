## This code computes the inverse of a matrix and cache its results to be later 
## used. If the inverse have already been calculated, the value will be cached.
## Otherwise, it will calculate the value.

## A is the matrix used to test this code. det(A) != 0, so inverse exists

A <- matrix( c(5,1,0,
               3,-1,2,
               4, 0, -1), nrow=3, byrow=TRUE)


## makeCacheMatrix creates a special vector, which contains a list containing a 
## function to set the value of the matrix, get the value of the matrix, set 
## the matrix inverse, and get the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
    
}


## cacheSolve calculates the matrix that is the inverse of the initial matrix, 
## checking first if the inverse have already been calculated before.

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

