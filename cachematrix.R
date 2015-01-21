## makeCacheMatrix
## Creates a list with four functions to set and get the values
## of the matrix and the inverset of it 
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(solve) m <<- solve
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## cacheSolve
## Calculates the inverse matrix.
## The first time a real computatation is performed (and the value stored)
## ... successive times the inverse is directly returned from the stored value
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
}


## An example of use will be (uncomment to execute if you want to see how it works)
#  a <-makeCacheMatrix()                    # First I create the vector of functions
#  a$set(matrix(c(1,2,3,0,1,4,5,6,0),3,3))  # Second I create a matrix
#  cacheSolve(a)                            # I solve it (caching the value)
#  cacheSolve(a)                            # I solve it again (this time I take the value from the cache)

