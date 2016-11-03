## Create two variables that store data in the parent environment. And then write two
## functions: one that points to them and the other that assigns the value/checks if 
## value is there. In this way we can avoid solving everytime.

## This function defines two variables and four functions to basically give us tags to 
## refer to when setting up the cache

makeCacheMatrix <- function(x = matrix()) {
                sol <- NULL
                set <- function(y) {
                        x <<- y
                        sol <<- NULL
                }
                get <- function() x
                setsolve <- function(solve) sol <<- solve
                getsolve <- function() sol
                list(set = set, get = get,
                     setsolve = setsolve,
                     getsolve = getsolve)
}


## This function goes into our reference variables to check if a value exists. Otherwise
## it executes and updates the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
                sol <- x$getsolve()
                if(!is.null(sol)) {
                        message("getting cached data")
                        return(sol)
                }
                data <- x$get()
                sol <- solve(data, ...)
                x$setsolve(sol)
                sol
}
