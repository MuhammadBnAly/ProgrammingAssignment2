#cachematrix.R

## a pair of functions that cache the inverse of a matrix.

## makeCacheMatrix
## This function creates a special object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve
## This function computes the inverse of makeCacheMatrix function above
## If the inverse has already been calculated , then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
	if(!is.null(inv)) {
	message("getting cached data")
		return(inv)
	}
	data <- x$get()
	inv <- inverse(data, ...)
	x$setinverse(inv)
	inv
}
