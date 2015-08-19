## Author: RSI
## Date: 8/19/15

### Overall Description: Assignment2 R-Prog Coursera ###
#
# Since Matrix inversion is usually a costly computation, 
# there may be some benefit to caching the inverse of 
# a matrix rather than compute it repeatedly.
# The two functions makeCacheMatrix and cacheSolve below are 
# used to cache the inverse of a matrix.
# 
# Note: For this assignment, the matrix supplied must always 
# be invertible.


### makeCacheMatrix function Description ###
#
# makeCacheMatrix creates a special "matrix" object 
# that can cache its inverse. It is a list containing a function to
# set the value of the matrix (must be a square invertible matrix)
# get the value of the matrix
# set the value of the inverse of matrix (obtained using solve())
# get the value of the cached inverse matrix


makeCacheMatrix <- function(x = matrix()) {
        ## Create a "matrix" object to cache its inverse
        
        # set cached inverse to NULL initially
        inv <- NULL
        
 
        set <- function(matrix) {
                # save new matrix and reset cached inverse to NULL
                
                x <<- matrix
                inv <<- NULL
        }
        
        
        get <- function() {
                # get the saved matrix
                
                x
        }
        
        
        setinv <- function(inverse) {
                # cache the inverse of the matrix
                
                inv <<- inverse
        }
        
        
        getinv <- function() {
                # get the cached inverse of the matrix 'x'
                
                inv
        }
        
        
        # return a list of the function operations       
        list(set = set, get = get, setinv = setinv, getinv = getinv)        
}


### cacheSolve function Description ###
#
# cacheSolve computes the inverse of the special "matrix"
# returned by makeCacheMatrix above. 
# If the inverse has already been calculated 
# (and the matrix has not changed), then the function retrieves 
# the inverse from the cache. Otherwise, it computes the inverse 
# of the matrix and sets the value in the cache.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        # get the cached inverse of x
        m <- x$getinv()
        
        if(!is.null(m)) {
                # retrieve cached inverse if it already exists
                
                message("getting cached data")
                return(m)
        }
        
        # otherwise, if no cached inverse, i.e. cached value of 
        # inverse is null, then:
        
        # 1. save the new matrix
        data <- x$get()
        
        # 2. compute the inverse of the square matrix using
        # solve function in R.
        m <- solve(data)
        
        # 3. cache computed inverse
        x$setinv(m)
        
        # 4. return the inverse
        m
}
