## Put comments here that give an overall description of what your
## functions do


## Function makeCacheMatrix takes user defined matrix and returns list of functions that set the matrix,
##get the matix, set the inverse value and get inverse value. Variable m contains the stored value. In the
## begining it set to NULL later function setinverse stors inverse value 

makeCacheMatrix <- function(x = matrix()) {
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


## Function cacheSolve sets the inverse matrix to local variable m. First it chackes if m is not NULL, if it
## true it will use cache data. Otherwise it will store the matrix to data, solve for inverse matrix, and set
## the inverse matrix to cache

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



