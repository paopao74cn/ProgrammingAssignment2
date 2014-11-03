## Matrix inversion is usually a costly computation and their may be some benefit to caching the inverse of a matrix rather than compute it repeatedly (there are also alternatives to matrix inversion that we will not discuss here). Your assignment is to write a pair of functions that cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y){
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setinv <- function(inv) m <<- inv
	getinv <- function() m
	list(set = set, get = get, setinv = setinv, getinv = getinv) 
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
## Computing the inverse of a square matrix can be done with the solve function in R. For example, if X is a square invertible matrix, then solve(X) returns its inverse.
## For this assignment, assume that the matrix supplied is always invertible.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
        if(!is.null(m)) {
        	message('getting cached matrix')
        	return(m)
        }
        mat <- x$get()
        m <- solve(mat, ...)
        x$setinv(m)
        m
}

## run in the console:
## > c=rbind(c(1, -1/4), c(-1/4, 1)) 
## > p <- makeCacheMatrix()
## > p$set(c)
## > cacheSolve(p)
## > invc <- solve(c)
## > p$setinv(invc)
## > p <- makeCacheMatrix()

