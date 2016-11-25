## These two functions together act as a bank of information to store and pull out of.
## The first function can store the information about the matrix, and the second function
## can set the inverse for the first function to store.


## This function keeps track of the inverse and allows other functions to gather
## information about the inverse. Other functions can get the inverse, set the 
## inverse, set the matrix or get the matrix with this function.

makeInverse <- function(x = matrix()) {
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


## This function will find the inverse of the matrix given to the makeInverse function
## if the inverse has not already been solved for. This can be detected by whether or
## not the makeInverse's getinverse() element is NA. If the inverse has not been found
## (aka makeInverse(x)$getinverse() is null) then this function will find the inverse
## and use the set function to set the inverse to the solved inverse.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        return(m)
}
