## Taken together, makeCachematrix and cachemean create
## a streamlined process for creating a square matrix and
## then inteverting it

## makeCacheMatrix is a function which sets and retrieves
## the inverse of the Matrix

makeCacheMatrix <- function(x = matrix() {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setSolve <- function(solve) s <<- solve
        getSolve <- function() s
        list(set = set, get = get, 
             setSolve = setSolve, 
             getSolve = getSolve)
        }


## cacheSolve takes the setSolve and getSolve values
## created from the makeCacheMatrix and then inverts

cacheSolve <- function(x, ...) {
       s <- x$setSolve()
       if(!is.null(s)) {
                message("getting cached data")
                return(s)
       }
        data <- x$get()
        s <- solve(data, ...)
        x$setSolve(x)
        s
}
