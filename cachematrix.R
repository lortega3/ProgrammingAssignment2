## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
Write the following functions:

# The makeCacheMatrix function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x) 
{
        
# Set up initial values
	inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x

# Define values that will be used by cacheSolve function
        setinv <- function(solve) inv <<- solve
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

## Write a short comment describing this function

# The cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix, referenced above. 
# If the inverse has already been calculated (and the matrix has not changed), then cachesolve retrieves the inverse from the cache.

# This function assumes that the matrix passed in inversable, as stated in the assignment description.

cacheSolve <- function(x, ...) 
{
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        
	# Run the inverse of the data, store it in inv
	inv <- solve(data, ...)
        
	# Initialize setinv
	x$setinv(inv)
        
	# Return the inverse of the matrix
	inv
}