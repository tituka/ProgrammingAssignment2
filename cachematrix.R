## The following functions create a matrix object and cache it's inverse, then return said cached inverse 
##if available, or calculates a new inverse.  


##This function creates a matrix object which can cache its inverse
makeCacheMatrix <- function(x = matrix()) { 
  minverse <- NULL                     
  ## function which sets matrix and resets inverse when this change happens
  set <- function(y) {                      
    x <<- y
    minverse <<- NULL              
  }
  ##returns matrix
  get <- function() x
  ##solves inverse of matrix
  setinverse <- function(solve) minverse <<- solve 
  ##returns inverse of matrix
  getinverse <- function() minverse        
  list(set = set, get = get,                    
       setinverse = setinverse,
       getinverse = getinverse)
}

# This function gets the inverse of the matrix from the cache when available, 
# or calculates it anew when it is not.
cacheSolve<- function(x, ...) {                 
  matrixinverse <- x$getinverse()
  #checks if inverse exists in cache.
  if(!is.null(matrixinverse)) {                 
    message("getting cached data - Inverse of the matrix")
    return(matrixinverse)} 
#if not, calculates it
  else{
data <- x$get()                               
matrixinverse <- solve(data, ...)
x$setinverse(matrixinverse)
matrixinverse
}}