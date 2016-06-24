## Inverse Computation of a matrix is generally a costly computation
## If we have to use the Inverse of a matrix several times,
## We can cache it, instead of re-computing

## makeCacheMatrix: this function takes a matrix and create a special matrix that can cache its inverse. 

makeCacheMatrix <- function(x = matrix()) {
    INV = NULL
    
    	# set: Function to "set"/"reset" a new input matrix 
	set <- function(y)
	{
	    x <<- y
	    INV <<- NULL
	}
	# get: Function to "output" the input matrix whose inverse has to be cached.
	get <- function() x
	# setINV: Set the inverse of input matrix
	setINV <- function(inv) INV <<- inv
	# getINV: Output the inverse on console
	getINV <- function() INV
	list(set = set, get = get,
             setINV = setINV,
             getINV = getINV)
}


## cacheSolve: compute the Inverse, if already computed then return the cached Inverse 

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

## Test Code to verify our code
mat <- makeCacheMatrix(matrix(1:4, 2, 2))
mat$get()
mat$getINV()
cacheSolve(mat)
cacheSolve(mat)
