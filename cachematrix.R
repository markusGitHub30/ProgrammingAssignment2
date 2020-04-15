## This functions can be used for caching the inversed matrix of an input matrix (must be symetric and inversable).
##This can lead to considerable performance increases in loops, because with the same input matrix over
##several loop runs the inverse matrix can be loaded and not recalculated for each run.

##makeCacheMatrix calculates the invere matrix and caches it.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
    list(set = set, get = get,
        setsolve = setsolve,
        getsolve = getsolve)
    
}


## cacheSolve checks if the invesed matrix is already cached (returns chached inversed matrix and text) 
##or not (calculate and return inversed matrix. 

cacheSolve <- function(x, ...) {
    m <- x$getsolve()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m
}
    
    
    




