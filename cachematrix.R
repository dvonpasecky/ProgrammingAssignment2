## This function creates a special "matrix" object that can cache its inverse.
## The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse

## This function is analogous to the makeVector example function; instead of caching the mean of a vector,
## it instead caches the inverse of a matrix using the solve function.

makeCacheMatrix <- function(x = matrix()) {
		m <- NULL
		set <- function(y){
			x << -y
			m <<- NULL
		}
		get <- function() x
		setmatrix <- function(solve) m<<- solve
		getmatrix <- function() m
		list(set = set, get = get,
			setmatrix = setmatrix,
			getmatrix = getmatrix)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve retrieves the inverse from the cache.

## This function is analogous to the cachemean example function; instead of returning the mean of the vector if it has already been cached,
## it instead returns the inverse of a matrix if it has already been cached.

cacheSolve <- function(x = matrix(), ...) {
		m <- x$getmatrix()
		if(!is.null(m)) {
				message("getting cached data")
				return(m)
		}
		matrix <- x$get()
		m <- solve(matrix, ...)
		x$setmatrix(m)
		m
}
