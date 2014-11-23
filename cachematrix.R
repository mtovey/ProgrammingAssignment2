## R Progamming Assignment 2 -
## Provide two functions that provide the capability to cache the inverse
## of a given matrix

## This function sets up a list containing getters/setters for a matrix (x) and the inverse of that matrix (s)
## s starts off as a NULL, then will get cached once setsolve gets called

makeCacheMatrix <- function(x = matrix()) {
			s <- NULL
            set <- function(y) {
                    x <<- y
                    s <<- NULL
            }
            get <- function() x
            setsolve <- function(solve) s <<- solve
            getsolve <- function() s
            list(set = set, get = get,
                 setsolve = setsolve,
                 getsolve = getsolve)
}


## This function leverages makeCacheMatrix to cache the inversion. It expects a list created
## via makeCacheMatrix and checks to see if the solve result is already there via getsolve. If so, returns that
## otherwise it computes the inverse via the solve function and caches it via setsolve

cacheSolve <- function(x, ...) {
        currentInverse <- x$getsolve()
        if(!is.null(currentInverse)) {
            message("getting cached matrix")
            return(currentInverse)
        }
        matrix <- x$get()
        currentInverse <- solve(matrix, ...)
        x$setsolve(currentInverse)
        currentInverse
}
