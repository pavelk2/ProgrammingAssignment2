## There are two functions:
## 		makeCahseMatrix()
##		cacheSolve()
## These functions help to cache inverse matrix computations,
## which can help to avoid redundant computations.

## This function creates a "matrix" object that can cache its inverse.
## It has 2 methods: 
## 		setsolve - to set the inverse matrix, 
##		getsolve - to retrieve the inverse matric

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
    set <- function(y) {
            x <<- y
            m <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## Computes the inverse of the "matrix" returned by makeCacheMatrix. 
## If the inverse has been calculated (and the matrix has not changed), 
## then the cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
	m <- x$getsolve()
	# if a cached matrix exists
    if(!is.null(m)) {
            message("getting cached data")
            return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    ## Return a matrix that is the inverse of 'x'
    m     
}
