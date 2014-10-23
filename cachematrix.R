## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Function creates matrix object that stores its inverse

makeCacheMatrix <- function(x = matrix()) {
	## init inverse matrix
	invM <- NULL

	## set matrix method
	setM <- function(matrix) {
		x <<- matrix
		invM <<- NULL
	}

	## get matrix method
	getM <- function() {
		x
	}

	## set inverse method
	setInvM <- function(inverse) {
		invM <<- inverse
	}

	## get inverse method
	getInvM <- function() {
		invM
	}

	## list function methods
	list(setM=setM, getM=getM, setInvM=setInvM, getInvM=getInvM)

}


## Write a short comment describing this function
## Compute the inverse of matrix from makeCacheMatrix function.
## If inverse is already calculated and matrix has not changed
## it returns cached version of inverse matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getInvM()

        ## return inverse if there is one in cache
        if(!is.null(inverse)) {
        	message('retriving inverse from cache...')
        	return(inverse)
        }

        ## get matrix 
        Mdata <- x$getM()

        ## calculate inverse 
        inverse <- solve(Mdata) 

        ## set inverse
        x$setInvM(inverse)

        ## return matrix
        inverse

}
