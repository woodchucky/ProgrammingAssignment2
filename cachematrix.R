## makeCacheMatrix creates a function that stores both a matrix and its 
## inverse, initializing the inverse to NULL.  It defines a "constructor"-like
## function set() as well as several utility functions such as getmatrix(),
## getinverse(), and setinverse() which do exactly as their names suggest
## for example, getinverse() retrieves the value of inverse stored in 
## makeCacheMatrix and setinverse() stores this value passed from a 
## calling function (which in this case will be cacheSolve)

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	mat <- x
	set <- function(y)  {
		mat <<- y
		inv <<- NULL
	}
	getmatrix <- function() mat
	setinverse <- function(inverse)  inv<<- inverse
	getinverse <- function() inv
	list(set=set, getmatrix=getmatrix, setinverse=setinverse,   getinverse=getinverse)
}


## cacheSolve checks to see if the matrix is null or identical to the last matrix
## that it was passed.  It does this by a call to the remember() function
## which contains a "content" attribute that acts as a static variable.  If
## the matrix is null or different from the last matrix it calculates the
## inverse and passes the inverse back to makeCacheMatrix and the new value 
## of the matrix to remember()

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of x'
		 newmatrix  <- x$getmatrix()
	if (is.null(remember(newmatrix)))
		{
		remember(newmatrix)
		inverse <- solve(newmatrix)
		answer <- x$setinverse(inverse)
		return(answer)
		}
	if(identical(remember(newmatrix),newmatrix))
		{
		remember(newmatrix)
		message("getting cached data")
		return(x$getinverse())
		}
	if (!(identical(remember(newmatrix),newmatrix)))
		{
		inverse <- solve(newmatrix)
		answer <- x$setinverse(inverse)
		return(answer)
		}
}

## The remember function is a helper function for cacheSolve as discussed above

remember <- function(z)  {
		y <- attr(remember, "content")
		if (is.null(y))  {
			a <- "b"
					}
		else {a <- "c"}
		attr(remember, "content") <<- z
		if (a == "b") return(NULL)
		if (a == "c") return(y)
		}
	
