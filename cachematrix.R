## Since matrix inversion can be an expensive computation, especially when 
## used in loops. The following two functions cache the inverse of a matrix
## thus reducing the cost of repeatedly computing. 

## Creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) inv <<- inverse
	getinverse <- function() inv
	list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Computes the inverse of the special matrix returned by "makeCacheMatrix".
## If the inverse has already been computed and the matrix remains unchanged,
## then the "cacheSolve" retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
	inv <- x$getinverse()
	if(!is.null(inv)) {
		message("getting cached data")
		return(inv)
	}
	data <- x$get()
	inv <- solve(data, ...)
	x$setinverse(inv)
	inv
}
