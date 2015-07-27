## This file contains the solution to Programming Assignment 2
## of Coursera's R Programming course.
##
## Matrix inversion can be a costly computation. To reduce the need to
## repeatedly calculate the matrix, this set of functions calculates
## the inverse of the matrix once, then caches the result for repeated use.
##
## Usage:
##      my_matrix  <- matrix(c(2, 4, 6, 8), nrow = 2, ncol = 2)
##      a <- makeCacheMatrix(my_matrix)
##
##      a$set(new_matrix) # Change the matrix to be cached. Reset inverse to NULL.
##      a$get()         # Return non-inverted matrix
##          [,1] [,2]
##      [1,]  2    6
##      [2,]  4    8
##
##      a$setinverse(solve(data,...)) # Called in cacheSolve() function. Uses solve()
##                                    # to calculate inverse of matrix.
##      a$getinverse()  # Returns the cached inverse of matrix


## makeCacheMatrix creates a special "matrix" object that can cache its inverse.
##
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Returns the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed),
## then return the inverse from the cache.
##
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
