############################################################################################
## Function makeCacheMatrix creates a special "matrix" object that can cache its inverse. ##
##                                                                                        ##
## Function cacheSolve computes the inverse of the special "matrix" returned by           ##
## makeCacheMatrix above. If the inverse has already been calculated then the cachesolve  ##
## retrieve the inverse from the cache.                                                   ##
##                                                                                        ##
## Author: Ahmed Awad                                                                     ##
## April 17 2020                                                                          ##
############################################################################################

## The makeCacheMatrix function will get and set the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
          x <<- y
          inv <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) inv <<- solve
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse
        )
}


## The cacheSolve function will verify if the matrix inverse is cached ,If the matrix is not null
## it will pop up a message "getting caching data" and return the inverse of the matrix


cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if (!is.null(inv)) {
          message("getting cached data")
          return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}

## For Testing :

## x = matrix(c(3,1,5,2,8,3,6,7,9), ncol=3, nrow=3)
## MCM = makeCacheMatrix(x)
## cacheSolve(MCM)
