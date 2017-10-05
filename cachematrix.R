## Here, I create a special pair of functions that cache the inverse of a matrix.

##I Create a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
 
        i <- NULL
        
        set <- function( matrix ) {
                m <<- matrix
                i <<- NULL
        }
        
        get <- function() {
                m
        }
        
        setInverse <- function(inverse) {
                i <<- inverse
        }
        
        getInverse <- function() {
                i
        }
        
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## The following function compute the inverse of the matrix(returned by makeCacheMatrix)
##If the inverse has already been calculated 
##then the cacheSolve should get the inverse from the cache and skip the computation.
##Otherwise, it calculates the inverse of the matrix and sets the value of the inversed matrix
##in the cache.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
         m <- x$getInverse()
        
        if( !is.null(m) ) {
                message("getting cached data")
                return(m)
        }
        
        data <- x$get()
        
        m <- solve(data) %*% data
        
        x$setInverse(m)
        
        m
}
