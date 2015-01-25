## There are two functions:
## 		makeCahseMatrix()
##		cacheSolve()
## These functions help to cache inverse matrix computations,
## which can help to avoid redundant computations.

## This function creates a "matrix" object that can cache its inverse.
## It has 2 methods: 
## 		setinverse - to set the inverse matrix, 
##		getinverse - to retrieve the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setinverse <- function(inverted_matrix) m <<- inverted_matrix
	getinverse <- function() m
	list(set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse)
}


## Computes the inverse of the "matrix" returned by makeCacheMatrix. 
## If the inverse has been calculated (and the matrix has not changed), 
## then the cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
	m <- x$getinverse()
	# if a cached matrix exists
	if(!is.null(m)) {
		message("getting cached data")
		return(m)
	}
	data <- x$get()
	m <- solve(data, ...)
	x$setinverse(m)
	## Return a matrix that is the inverse of 'x'
	m     
}
