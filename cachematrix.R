## Coursera assignment: Caching the Inverse of a Matrix

## Creates a special matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL # inverse matrix
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	
	get <- function() x
	
	setinv <- function(inv) i <<- inv
	
	getinv <- function() i
	
	list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## Calculates the inverse matrix of the matrix created by makeCacheMatrix

cacheSolve <- function(x, ...) {
	i <- x$getinv()
	if (!is.null(i)) {
		message("getting cached matrix")
		return(i) # return the cached inverse matrix
	}
	data <- x$get()
	i <- solve(data)   # Return a matrix that is the inverse of 'x'
	x$setinv(i)
	i
}
