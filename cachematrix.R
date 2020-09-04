## These "special" functions cache the object (a matrix) recieved
## as argument and "process" the information in it This information 
## is accessed through a list. If makeCacheFuncion function doesn't 
## return the inverse of the matrix, you can use the cacheSolve function, 
## which indeed will process the matrix and returns its inverse.

## This function creates a "special" matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        set_inv <- function(inversa) i <<- inversa
        get_inv <- function() i
        list(set = set, get = get,
             set_inv = set_inv,
             get_inv = get_inv)

}


## This function computes the inverse of the special 
##"matrix" returned by makeCacheMatrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$get_inv()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$set_inv(i)
        i
}
