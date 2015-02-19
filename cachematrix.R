## Álvaro García Recuero
## Pair of functions to cache the inverse of a matrix

## Write a short comment describing this function
makeCacheMatrix <- function( x = matrix() ) {
    
    # Check if input is matrix
    if (!is.matrix(x)){
        stop("Please enter a matrix")
    }
    
    # Initialize inverse matrix
    local.inverse <- NULL
    
    # Store set value of matrices
    set <- function(y) {
        x <<- y
        local.inverse <<- NULL
    }
    
    # Get stored matrix
    get <- function() x
    
    # Set/Get inverse of matrix
    setinverse <- function(inverse) {
        local.inverse <<- inverse
    }
    getinverse <- function() {
        local.inverse
    }
    
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## Cache for matrix inverse, if exists reuse it, else compute it.
cacheSolve <- function(cached, ...) {
    
    inverse <- cached$getinverse()
    
    # If cached, display message
    if(!is.null(inverse)) {
        message("getting cached inverse matrix...")
    } else{
        ## Return a matrix that is the inverse of 'x'
        data <- cached$get()
        inverse <- solve(data, ...)
        cached$setinverse(inverse)
    }
    # Finally return current value of the inverse
    inverse
}
