
# ===========================================================================
# The present script aims to compute the matrix inversion  and cache the 
# result so it might have considerable gain in performance.
# ===========================================================================
#
# ===========================================================================
# The function below creates a object of matrix type and cache its inverse.
# It is built from the function MakeVector
# ===========================================================================
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


# ===========================================================================
# The function below compute the matrix inversion and cache its result.
# It is built from the function cachemean
# ===========================================================================

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
