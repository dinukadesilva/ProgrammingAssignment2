## Put comments here that give an overall description of what your
## functions do

## Take the x matrix as a input\
## wrap the x matrix with getters and setters

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	set <- function(y) {
		x <<- y
		i <<- NULL
	}
	get <- function() x
	setInverse <- function(inverse) i <<- inverse
	getInverse <- function() i
	  
	list(set = set, get = get,
		setInverse = setInverse,
		getInverse = getInverse)
}


## If the invers has been cached it is loaded
## otherwise it's created and send to the cache as well

cacheSolve <- function(x, ...) {
	i <- x$getInverse()
	if(!is.null(i)) {
		message("getting cached data")
		return(i)
	}
	
	data <- x$get()
	i <- solve(data, ...)
	x$setInverse(i)
	i
}
