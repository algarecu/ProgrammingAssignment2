## Álvaro García Recuero
## Pair of functions to cache the inverse of a matrix

## Write a short comment describing this function
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    
    setinverse <- function(inverse) m <<- solve(x)
    getinverse <- function() m
    
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## Cache for matrix inverse, if exists reuse it, else compute it.
cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached inverse matrix")
        ## Return a matrix that is the inverse of 'x'
        return(m)
    }
    data <- x$get()
    m <- inverse(data, ...)
    x$setinverse(m)
    ## Return a matrix that is the inverse of 'x'
    m
}