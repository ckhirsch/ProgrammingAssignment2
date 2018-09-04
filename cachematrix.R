## makeCacheMatrix: creates a special "matrix" that can cache its inverse.


makeCacheMatrix <- function(x = matrix()) {
        matrix_inverse = NULL
        set = function(y) {
                x <<- y
                matrix_inverse <<- NULL
        }
        get = function() {
                x
        }
        setinv = function(inverse) {
                matrix_inverse <<- inverse 
        }
        getinv = function() {
                matrix_inverse
        }
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}

##cacheSolve: returns the inverse of the matrix returned by makeCacheMatrix(); 
##            if the inverse has previously been computed and has not changed, 
##            the already cached solution will be retrieved


cacheSolve <- function(x, ...) {
        matrix_inverse = x$getinv()
        if (!is.null(matrix_inverse)){
                message("getting cached data")
                return(matrix_inverse)
        }
        data = x$get()
        matrix_inverse = solve(data, ...)
        x$setinv(matrix_inverse)
        matrix_inverse
}