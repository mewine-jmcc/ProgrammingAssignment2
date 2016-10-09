## The two functions compute the inverse of a square matrix and
## cache it. 

## This function creates a special "matrix" object that can cache
## its inverse.

makeCacheMatrix <- function(x = matrix()) {
    iv <- NULL
    set <- function(y) {
        x <<- y
        iv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) iv <<- inverse
    getinverse <- function() iv
    list(set = set, get = get, 
         setinverse = setinverse, 
         getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" 
## returned by mackCacheMatrix above. If the inverse has already
## been calculated (and the matrix has not changed), then the
## cachesolve should retrieve the inverse from the cache. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    iv <- x$getinverse()
    if(!is.null(iv)){
        message("getting cached data")
        return(iv)
    }
    data <- x$get()
    iv <- solve(data)
    x$setinverse(iv)
    iv
}
