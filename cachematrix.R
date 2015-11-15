## Put comments here that give an overall description of what your
## functions do

## The following function takes in a matrix x, and then allows the 
## following methods to work on the object:
## get the matrix x
## either calculate the inverse of matrix x passed by the function call, or
## calculate the inverse of matrix x passed by the set method
## the setInverse method is the matrix inverse calculation operaton
## and the getInverse method is to get the inverse from cache.

makeCacheMatrix <- function(x = matrix()) {
	  m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(x) m <<- solve(x)
        getInverse <- function() m
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## The following funcction is an if-then routine.
## It first checks the cache to see if the matrix inverse already exists.
## If it does, it grabs the matrix inverse already in cache and returns 
## the matrix inverse, without doing the matrix inverse calculation.
## Otherwise, it calculates the matrix inverse local to the function, puts 
## it into cache and returns it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	
        		m <- x$getInverse()
        		if(!is.null(m)) {
                		message("getting cached data")
                		return(m)
 			 }       		
        	data <- x$get()
        	m <- solve(data)
        	x$setInverse(m)
        	m
	
}
