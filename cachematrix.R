setwd("C:/Users/Sarah-Zakhia/Desktop/R Programming/Week3/")

##The assignment is to write two functions, one which will create a special matrix
##object that can cache its own inverse 'makeCacheMatrix' and one which will compute
##the inverse of the special matrix returned by makeCacheMatrix. if the matrix did not
##change and the inverse has already been calculated then the inverse is retrieved
##from Cache.


## makeCacheMatrix will create a special matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix(rnorm(100), 10, 10)) {
        i <- NULL
        set <- function(y){
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) i <<- solve
        getinverse <- function() i
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve will compute the inverse of the special matrix returned by makeCacheMatrix. if the matrix did not change and the inverse has already been calculated then the inverse is retrieved from Cache.
## Use the solve() function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached inverse of matrix")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
