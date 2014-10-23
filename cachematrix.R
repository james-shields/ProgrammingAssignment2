## the makeCacheMatrix function creates a matrix object that can cache its inverse
## the setCacheMatrix function calculates the inverse of the matrix object created in
## if the inverse has not been previously calculated, otherwise it returns the cached
## value

## this function creates a special matrix object.  It contain s methods for setting
## the value of the matrix (set), returning the values of the matrix (get),
## setting the inverse (setInverse), and returning the value of the inverse (getInverse)

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    
    get <- function() x
    setInverse <- function(solve) inverse <<- solve
    getInverse <- function() inverse
    list(set = set, get = get,
         getInverse = getInverse,
         setInverse = setInverse)
}


## this function checks a matrix object (x) to determine if an inverse has been 
## calculated for it.  If the inverse has not been calculated, then it calculates it
## otherwise, it returns the cached value of the inverse

cacheSolve <- function(x, ...) {
    inverse <- x$getInverse()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    
    data <- x$get()
    inverse <- solve(data)
    x$setInverse(inverse)
    inverse
}
