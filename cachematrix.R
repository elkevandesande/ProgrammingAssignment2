## These functions are written for programming assigment 2 in
## in the Coursera Course about R Programming

## These functions are based on the makeVector example in the 
## programming assigment. The makeCacheMatrix function creates
## a special "matrix"object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
 	m <- NULL
 	set <- function(y){
  		x <<- y
  		m <<- NULL
	}
	get <- function() x
	setmatrix <- function(solve) m <<- solve
	getmatrix <- function() m
	list(set=set, get=get,
   		setmatrix=setmatrix,
   		getmatrix=getmatrix)
}

## The cacheSolve function computes the
## inverse of the special "matrix" returned by makeCacheMatrix 
## above. If the inverse has already been calculated (and the 
## matrix has not changed), then cacheSolve retrieves 
## the inverse from the cache.

cacheSolve <- function(x=matrix(), ...) {
    	m <- x$getmatrix()
    	if(!is.null(m)){
      	message("getting cached data")
      	return(m)
    	}
    	matrix <- x$get()
    	m <- solve(matrix, ...)
   	 x$setmatrix(m)
   	 m
}


