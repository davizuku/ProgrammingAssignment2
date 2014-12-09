## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
	inverse <- NULL
	set <- function(y) {
		x <<- y
		inverse <<- NULL
	}
	get <- function() x
	setInverse <- function(inv) inverse <<- inv
	getInverse <- function() inverse
	
	list (set = set, get = get, 	
		setInverse = setInverse, 
		getInverse = getInverse);
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
	inv <- x$getInverse()
	if (!is.null(inv)) {
		message("getting cached data")
		return(inv)
	}
	data <- x$get()
	inv <- solve(data, ...)
	x$setInverse(inv)
	inv
}



cachedMatrixTest <- function(n) {
	message("Creating original matrix...")
	print(system.time(m <- matrix(rnorm(n*n), c(n, n))))
	message("Matrix created with random numbers")
	message("------------")
	message("Inverting the matrix with solve()...")
	print(system.time(inv1 <- solve(m)))
	message("Inverted matrix in variable inv1")
	message("------------")
	message("Creating cachedMatrix object...")
	print(system.time(cacheMat <- makeCacheMatrix(m)))
	message("CachedMatrix object created")
	message("------------")
	message("Inverting the matrix with cacheSolve()...")
	print(system.time(inv2 <- cacheSolve(cacheMat)))
	message("Inverted matrix in variable inv2")
	message("------------")
	message("Inverting the matrix with cacheSolve()...")
	print(system.time(inv3 <- cacheSolve(cacheMat)))
	message("Inverted matrix in variable inv3")
	message("============")	
	message("Results: ")
	message("identical(inv1, inv2): ", appendLF = FALSE)
	message(identical(inv1, inv2))
	message("identical(inv2, inv3): ", appendLF = FALSE)
	message(identical(inv2, inv3))
}

