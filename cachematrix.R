## The makeCacheMatrix function takes a Matrix and returns
## a list of 4 functions:  set(y), get(), setinverse(i), and getinverse()

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(i) inverse <<- i
        getinverse <- function() inverse
        list(set = set, get = get, 
             setinverse = setinverse, getinverse = getinverse)
}

## cacheSolve(x) takes a Matrix and returns the inverse of that matrix.
## If the inverse matrix has already been calculated, it returns a cahced copy
## of the inverse matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getinverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data)
        x$setinverse(inverse)
        inverse
}