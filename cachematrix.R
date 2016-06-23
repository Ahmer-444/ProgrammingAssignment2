## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    INV = NULL
	set <- function(y)
	{
	    x <<- y
	    INV <<- NULL
	}
	get <- function() x
	setINV <- function(inv) INV <<- inv
	getINV <- function() INV
	list(set = set, get = get,
             setINV = setINV,
             getINV = getINV)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        INV <- x$getINV()
        if(!is.null(INV))
        {
	    message("Get Cached Inverse")
	    return(INV)
        }
        
        data <- x$get()
        INV <- solve(data,...)
        x$setINV(INV)
        INV
        
}

## Test Code
mat <- makeCacheMatrix(matrix(1:4, 2, 2))
mat$get()
mat$getINV()
cacheSolve(mat)
cacheSolve(mat)