## Put comments here that give an overall description of what your
## functions do

## Example usage:
## x<-hilbert(2)   
##      or: x<-matrix(c(1.0, 0.5, 0.5, 0.3), nrow=2, ncol=2)
## m<-makeCacheMatrix(x)
## m$get() 
## m$getInverse()
## cacheSolve(m)
## m$getInverse()

## Create square matrix
hilbert <- function(n) { i <- 1:n; 1 / outer(i - 1, i, "+") }


## makeCacheMatrix: This function creates a special "matrix" object 
## that can cache its inverse.
## The matrix should be a square matrix. 

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    
    ## initialize variables
    set<-function(y) {
        x<<-y
        m<<-NULL
    }
    get<-function()x
    
    setInverse <- function(matrix) { 
        m <<- matrix
    }
    
    getInverse <- function() {
        m
    }
    
    list(set=set, 
         get=get,
         setInverse=setInverse,
         getInverse=getInverse)

}

## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data)
    x$setInverse(m)
    m
}

