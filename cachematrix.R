## 'makeCacheMatrix' is a fxn that takes a matrix as an argument
## and caches its attributes in memory
## 'cacheSolve' is a fxn that attempts to check for an object
## in memory and if it exists, returns the inverse of it

## a function that persists the contents and attributes of
## a matrix while also producing its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    get <- function() x
    setinverse <- function(solve) inv <<- solve
    getinverse <- function() inv
    list( set = set, get = get, 
          setinverse = setinverse, 
          getinverse = getinverse)
}


## a function that retrieves an object from memory and, if it exists,
## gains access to its attributes while also reserving the capability
## to populate a new matrix object with its own attributes

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message('getting cached data')
        return(inv)
    }
    ## get new matrix
    new_mat <- x$get()
    ## take the inverse of the matrix
    inv <- solve(data, ...)
    ## set the new inverse
    x$setinverse(inv)
    inv
}
