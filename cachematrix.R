## Put comments here that give an overall description of what your
## functions do
##  Below are two functions that are used to create a special object 
## that stores a matrix and cache's its inversion

## Write a short comment describing this function
## The first function, makeCacheMatrix creates a special "matrix", 
## which is really a list containing a function to
## - set the value of the matrix
## - get the value of the matrix
## - set the value of the inversion
## - get the value of the inversion

makeCacheMatrix <- function(x = matrix()) {

  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
## The following function calculates the inversion of the special "matrix" 
## created with the above function. However, it first checks to see 
## if the inversion has already been calculated. If so, it gets 
## the inversion from the cache and skips the computation. 
## Otherwise, it calculates the inversion of the data and sets the value of the inversion
## in the cache via the setinversion function.

cacheSolve <- function(x, ...) {
  
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
        ## Return a matrix that is the inverse of 'x'
}
