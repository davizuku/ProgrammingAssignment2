## Put comments here that give an overall description of what your
## functions do
# This document creates a way to store the previous
# calculations of the inverse of a matrix. 
# The system consists on two functions, one for 
# defining the "special matrix" taking profit of 
# R lexical scoping (makeCacheMatrix). The other
# uses the data stored in the custom matrix to 
# compute or not the inverse of the matrix (cacheSolve). 


## Write a short comment describing this function
# This function creates a special "matrix", which is really
# a list containing a function to: 
#	1. set the value of the matrix
# 	2. get the value of the matrix
#	3. set the value of the inverted matrix
#	4. get the value of the inverted matrix
# Taking profit of the lexical scoping, the function
# stores the value of the original matrix 'x' in the
# get() method, since it is the value of the variable
# in the environment when it is first created. 
# Similar process is followed to keep track of the value
# of the 'inverse' variable.
# The other methods use the operator <<- to modify the 
# stored variables. 
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
# This function calculates the inverse of the special 
# "matrix" created with the above function. 
# In order to take profit of the variables stored in it, 
# this function first checks if the inverted matrix has
# already been calculated and stored in the special 
# "matrix". If it is, then it is returned as the result, 
# otherwise it is computed with the original "solve(...)"
# method. 
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


# This function has been created to test the correctness
# of the two previous functions. 
# First, a squared matrix is created and filled with random
# numbers. 
# After that, a usual invertion is performed and its profiling
# is printed on the console. 
# Then, two new inversions are calculated using the original
# matrix, but now through the cacheSolve function. 
# Results show that the first time cacheSolve is called, 
# it consumes the same amount of time as the original function.
# However, next call is instantly resolved due to the cache.
# The two different ways produce the same result as it is 
# proved using the identical function.
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

