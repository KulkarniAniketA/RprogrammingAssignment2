# Two functions which contains the inverse of a matrix

# This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function( mat = matrix() ) {

	# Initialization of inverse
    initial <- NULL

    # Set the matrix
    set <- function( matrix ) {
			mat <<- matrix
			initial <<- NULL
	}

    # Get the matrix
    get <- function() {
		mat
	}

    # Set the inverse of the matrix
    setInverse <- function(inverse) {
        initial <<- inverse
	}

    # Get the inverse of the matrix
    getInverse <- function() {
		initial
	}

    # List of the methods can be performed on matrix
    list(set = set, get = get,
		setInverse = setInverse,
		getInverse = getInverse)
}


# This function computes the inverse of the special "matrix" returned by 
# "makeCacheMatrix" above.  If the inverse has already been calculated 
# (and the matrix has not changed), then the cachesolve should retrieve 
# the inverse from the cache.

cacheSolve <- function(x, ...) {

    # Get a matrix which is the inverse of 'x'
    mat <- x$getInverse()

    # Return of the inverse when it is already exist
    if( !is.null(mat) ) {
            message("getting cache data")
            return(mat)
    }

    # Retrieve the matrix from the created object
    data <- x$get()

    # Compute the inverse of the matrix using matrix multiplication
    mat <- solve(data) %*% data

    ## Set the inverse of the matrix to the created object
    x$setInverse(mat)

    ## Get the original matrix
    mat
}