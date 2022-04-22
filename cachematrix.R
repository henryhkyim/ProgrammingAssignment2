## The following pair of functions are used to solve the inverse of given matrix. 
## The results will be cached for future use to avoid repeating the computation.

## makeCacheMatrix is a constructor function, which create a list of get and set 
##   functions of the data matrix. It also cache the corresponding inverse 
##   matrix together with the getinverse and setinverse function for caching
##   purpose

makeCacheMatrix <- function(m = matrix()) {
        my_inverse <- NULL
        set <- function(y) {
                m <<- y
                my_inverse <<- NULL
        }
        get <- function() m
        setinverse <- function(inverse) my_inverse <<- inverse
        getinverse <- function() my_inverse
        list(set = set, 
             get = get, 
             setinverse = setinverse, 
             getinverse = getinverse)
}


## cachesolve first check if there is cached result for inverse of matrix x
## cached result will be returned if it is available
## otherwise find inverse by using solve function, then cache for future use,
##   and then return the result

cacheSolve <- function(cached_matrix, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- cached_matrix$getinverse()
        if (!is.null(inverse)) {
                message("getting cache data")
        }
        data <- cached_matrix$get()
        inverse <- solve(data, ...)
        cached_matrix$setinverse(inverse)
        inverse
}
