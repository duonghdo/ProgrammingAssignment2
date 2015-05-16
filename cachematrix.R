## The first function creates an object to store a matrix and
## its inverse. The second function will compute and return
## the inverse of a matrix, it first checks for cached 
## results, if no result exists, it will compute the inverse.

## Create an object that stores a matrix and its inverse.
## Create a list containing functions to
## 1.  set the value of the matrix
## 2.  get the value of the matrix
## 3.  set the value of the inverse
## 4.  get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y){
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setinv <- function(solve) inv <<- solve
	getinv <- function() inv
	list(set = set, get = get,
		setinv = setinv, getinv = getinv)
}


## Look for inverse of a matrix in cache, if there is no cached
## data, calculate and return the inverse for the matrix

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)){
    	message("getting cached data")
    	return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinv(inv)
    inv
}
