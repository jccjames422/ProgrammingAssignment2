## The makeCacheMatrix function takes a Matrix and returns
## a list of 4 functions:  set(y), get(), setinverse(i), and getinverse()

makeCacheMatrix <- function(x = matrix()) {
        # Takes a matrix, saves it, and creates four functions
        # that operate on that matrix.
        #
        # Args:
        #       x: Must be a square numerical matrix.
        #
        # Returns:
        #       A list of four fuctions that operate on the
        #       given matrix.  The matrix itself is saved
        #       in this function's environment.
        #       
        #       set(y):
        #               takes a square numerical matrix and
        #               saves it in the variable x
        #
        #       get():
        #               returns the data stored in x
        #
        #       setinverse(i):
        #               takes a square numerical matrix and
        #               saves it in the variable inverse.  Note
        #               that this function does NOT inverse
        #               the matrix passed in by the argument x.
        #
        #       getinverse():
        #               returns the data stored in inverse
        
        inverse <- NULL
        
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        
        get <- function() x
        
        setinverse <- function(i) inverse <<- i
        
        getinverse <- function() inverse
        
        return(list(set = set, get = get, 
             setinverse = setinverse,
             getinverse = getinverse))
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