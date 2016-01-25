## R - Programming Assignment 2
## Design two helper functions to cache Matrix Inverse calculation

## Special Matrix make function getters/setters properties to store/retrieve
## matrix and its inverse
## Returns a list with of callable properties/methods etc

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setInverse <- function(inverse) m <<- inverse
	getInverse <- function() m
	list(set = set, get = get,
	     setInverse = setInverse, 
	     getInverse = getInverse)
}


## Cached Matrix inverse calculation method which takes in makeCacheMatrix as the input,
## checks if the Inverse is aleady computed, if so return caches inverse.
## If the cache is stale, compute the inverse using (solve), update cache with inverse and
## return the inverse.

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
