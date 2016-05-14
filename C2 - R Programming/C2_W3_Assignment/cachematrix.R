## makeCacheMatrix provides auxiliary methods of storing / loading the matrix with
## its inverted(solved) value

## The function exposes four methods: get, set, get_solved and set_solved as a list
## object

makeCacheMatrix <- function(x = matrix()) {
	solved <- NULL
	
	set <- function(y) {
		x <<- y
		solved <<- NULL
	}
	
	get <- function() {
		x
	}
	
	set_solved <- function(solved_matrix) {
		solved <<- solved_matrix
	}
	
	get_solved <- function() {
		solved
	}
	
	list(set = set, get = get, set_solved = set_solved, get_solved = get_solved)
}


## cacheSolve returns the solved value of a "CacheMatrix" object passed as a
## parameter. In case the solved value was calcualted before, its cached value is
## returned, otehrwise it is calculated again.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	
	solved <- x$get_solved();
	if (!is.null(solved)) {
		message("getting cached data")
		return(solved)
	}
	
	data <- x$get()
	data_solved <- solve(data, ...)
	x$set_solved(data_solved)
	data_solved
}
