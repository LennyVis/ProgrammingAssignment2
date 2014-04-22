## makeCacheMatrix and cacheSolve are functions which calculate the inverted matrix of a given matrix
##After the result is calculated, it is saved in the cache and is accessible for repeated use given that the initial matrix has
##not changed

## makeCacheMatrix is a function which manages the caching of a matrix and its inverted matrix in the memory
## makeCacheMatrix deals with writing and reading the matrixes and returns a vector of functions which gives
## read and write access to the saved values

makeCacheMatrix <- function(mainMatrix = matrix()) {
									inverseMatrix <- NULL
									
        							set <- function(mainMatrix.param) {
                									mainMatrix <<- mainMatrix.param
                									inverseMatrix <<- NULL
        							}
        							
        							get <- function() mainMatrix
        							
        							setinverse <- function(inverseMatrix.param) inverseMatrix <<- inverseMatrix.param
        							
        							getinverse <- function() inverseMatrix
        							
        							list("set" = set, "get" = get,
             						"setinverse" = setinverse,
             						"getinverse" = getinverse)
}


##cacheSolve calculates the inverse matrix of a given matrix by first checking the memory for a cashed result
##If a cached result is available, the function extracts it, otherwise the inverted matrix is calculated and is cached in the
##memory

cacheSolve <- function(mainMatrix, ...) {
		 				## Return a matrix that is the inverse of 'x'
						inverseMatrix <- mainMatrix$getinverse()
						
        					if(!is.null(inverseMatrix)) {
                								message("getting cached data")
                								return(inverseMatrix)
        					}
        					dataMatrix <- mainMatrix$get()
        					inverseMatrix <- solve(dataMatrix, ...)
        					mainMatrix$setinverse(inverseMatrix)
        					inverseMatrix
}
