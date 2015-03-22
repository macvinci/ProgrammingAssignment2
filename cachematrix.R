## This couple of function allow to calculate the reverse of a matrix 
## and put the result in a cache

## makeCacheMatrix function creates a special "matrix", which is really a list
## containing a function to set the value of the vector, get the value of the vector,
## set the value of the mean, get the value of the mean.



makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinvmat <- function(invmat) m <<- invmat
        getinvmat <- function() m
        list(set = set, get = get,
             setinvmat = setinvmat,
             getinvmat = getinvmat) 
}


## cacheSolve function calculates the reverse of the special "matrix" created with the above function.
## However, it first checks to see if the inverse has already been calculated. If so, it gets the inverse from the cache
## and skips the computation. Otherwise, it calculates the inverse of the matrix and sets the value of the inverse
## in the cache via the setinvmat function.

cacheSolve <- function(x, ...) {
        m <- x$getinvmat()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinvmat(m)
        m
        ## Return a matrix that is the inverse of 'x'
}
