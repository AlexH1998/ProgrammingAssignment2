
makeCacheMatrix <- function( m = matrix()){
    # Initialize inverse
    inv <- NULL
    # Setter for the matrix
    set <- function(matrix){
        x <<- matrix
        inv <<- NULL
    }
    # Getter for the matrix
    get <- function(){
        x
    }
    # Set the inverse
    setinv <- function(inverse){
        inv <<- inverse
    }
    # Get the inverse
    getinv <- function(){
        inv
    }
    # Return list of methods
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}

cacheSolve <- function(x, ...){
    # Return inverse of x
    inv <- x$getinv()
    # Return only inverse if already set
    if (!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    # Get matrix from data object
    mat <- x$get()
    # Calculate the inverse through multiplication
    inv <- solve(mat) %*% mat
    # Set inverse to the object
    x$setinv(inv)
    # Return the matrix
    inv
}
