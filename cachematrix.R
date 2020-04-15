## Matrix inversion is usually a costly computation and there is a benefit of caching 
#the inverse of a matrix and serve from the cache rather than compute it repeatedly
## every time. 
##
##This program will cache the calculated inverse of a matrix and serve from the cache 
##when it is requested again. 


## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    m_inverse <- NULL
    
    set <- function(y)
    {
        x <<- y
        m_inverse <<- NULL
    }
 
    get <- function() x
    
    getInverse <-  function() m_inverse
    
    setInverse <- function(inverse) m_inverse <<- inverse
    
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This fucntion calculae the inverse of a matrix returned by makeCacheMatrix function. 
## If the inverse is already calculated, this will retrieve it from the cache.

cacheSolve <- function(x, ...) {
    ##Reading from the cache.
    m_inv <- X$getInverse()
    
    if(!is.na(m_inv))
    {
        ##Data is already set in cache.
        return(m_inv)
    }
    
    m_data <- x$get()
    m_inv <- solve(m_data, ...)
    
    ##Set inverse to cache.
    x$setInverse <- m_inv
    
    m_inv
}
