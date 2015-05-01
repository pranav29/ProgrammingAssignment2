## Write the following functions:
##makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
##cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

## makeCacheMatrix function will create a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        
        ## setting the "x" to the value of object "y"
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        ##This function returns the value of "x" when it is called
        get <- function() x
        
        ##This function stores the inverse of matrix "x"
        setInverse <- function(mean) m <<- mean
        
        ##This function returns the value of inverse of matrix "x"
        getInverse <- function() m
        
        
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)

}


## cacheSolve function will calculate the inverse of matrix 'x' and return its value

cacheSolve <- function(x, ...) {
        m=x$getInverse()
        
        ##if the value of inverse of matrix is already cached
        if(!is.null(m)){
                message("returning cache value of inverse matrix")
                return(m)
        }
        
        ##if inverse value is not cached
        data=x$get()
        m=solve(data)
        x$setInverse(m)
        m
}
