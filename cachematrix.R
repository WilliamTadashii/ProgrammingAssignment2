## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        ## Creates a object matrix with functions to get and set the inverse 
	m <- NULL
	set <- function(y){
	  x <<- y
	  m <<- NULL
	}
	get <- function() x
	setinv <- function(inv) m <<- solve(x)
	getinv <- function() m
	list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	m <- x$getinv()
	if(!is.null(m){
	  message("Getting cached data")
	  return(m)
	}
	message("Computing the inverse matrix")
	data <- x$get()
	m <- solve(data)
	x$setinv(m)
	m:
}
