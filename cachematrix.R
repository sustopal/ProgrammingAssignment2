## Functions to save and calculate the inverse of a square matrix

## makeCacheMatrix: This function creates an "object" to contain a matrix and 
## its inverse.
## Argument: x, matrix to be cached and "inversed"
## Return: list of functions to handle with the "cache"
makeCacheMatrix <- function(x = matrix()) {
    
    inv <- NULL #initialize the inverse matrix to NULL
    
    set <- function(y) {
        x <<- y         #set the matrix
        inv <<- NULL    #the matrix has been changed so the inverse must be NULL
    }
    
    get <- function() x     #return the cached matrix
    
    setinverse <- function(inverse) inv <<- inverse     #set the inverse
    
    getinverse <- function() inv    #get the inverse
    
    #return the list of functions: the cached object
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)

}


## cacheSolve: This function calculates the inverse of a matrix and save it in 
## the "cache" 
## Argument: cache matrix object
## Return: the inverse matrix of x
cacheSolve <- function(x, ...) {
            
    inv <- x$getinverse() 
    
    if(!is.null(inv)) {     #the inverse has already been calculated
        message("getting cached inverse matrix")
        return(inv)
    }
    
    #the inverse has not been calculated yet
    
    data <- x$get()
    inv <- solve(data, ...)     #calculate the inverse
    x$setinverse(inv)           #save the inverse in the cache
    
    inv
    
}
