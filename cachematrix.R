## The following two functions will create an R object to store a matrix and cache its inverse using lexical scoping

## Initial function called to store a matrix object and list of functions.
## An emptry matrix is create if no parameters are provided.
## Example : v <- makeCacheMatrix(matrix(c(1,2,3,4), nrow=2, ncol=2))

makeCacheMatrix <- function(x = matrix()) {
	# Initialize variable to store inverse
        i <- NULL
	# Update internal variable with new matrix and reset inverse
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
	# Return matrix
        get <- function() x
	# Set and cache inverse
        setSolve <- function(solve) i <<- solve
	# Return inverse of matrix
        getSolve <- function() i
	# Create vector of available functions for this object
        list(set = set, get = get, setSolve = setSolve, getSolve = getSolve)
}

## Function to calculate inverse of matrix created with makeCacheMatrix.
## Calculates inverse or returns cached inverse if it exists
## Example : cacheSolve(v)

cacheSolve <- function(x, ...) {
	# Get inverse 
        i <- x$getSolve()
	# Determine if inverse exists and return if it exists
        if(!is.null(i)) {
		# Return cached inverse
                message("getting cached data")
                return(i)
        }
	# Otherwise copy matrix into local variable
        data <- x$get()
	# Calaculate inverse
        i <- solve(data, ...)
	# Set (and cache) calculated inverse
        x$setSolve(i)
	# Return inverse
        i
}