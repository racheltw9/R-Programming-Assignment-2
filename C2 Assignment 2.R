## create an object that stores "get" and "set" functions for the matrix x and it's inverse matrix m

makeCacheMatrix<- function(x = matrix())  {
	inv<- NULL
	set<- function(y) {
		x<<-y
		inv<<- NULL
	}
	get<- function() x
	setInverse<- function(inverse) inv<<- inverse
	getInverse<- function() inv
	list(set = set,
		 get = get,
		 setInverse = setInverse,
		 getInverse = getInverse)
}

## This function takes as input and output from the makeCacheMatrix function above.

cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
	inv<- x$getInverse()
	if(!is.null(inv)) {
		message("getting cached data")
		return(inv)
	}
	mat<- x$get()
	inv<- solve(mat, ...)
	x$setInverse(inv)
	inv
}