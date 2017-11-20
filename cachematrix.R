## The following functions: makeCacheMatrix and cacheSolve will cache
## the inverse of a matrix

## makeCacheMatrix creates a special "matrix" that can cache its inverse

makeCacheMatrix <- function(x=matrix())   {
  invmatrix <- NULL
  set <- function(y) {
      x <<- y
      invmatrix <<- NULL
  }
  
  get <- function() x #define get fxn that returns values of the matrix
  setinv <-function(inverse) invmatrix <<- inverse #sets value of invmatrix in PARENT environment
  getinv <- function() invmatrix  #this gets the value of matrix where the function is CALLED
  list(set=set, get=get, setinv=setinv, getinv=getinv)
  
}


## This function, cacheSolve, computes the inverse of the "matrix" created
## above and if the inverse has already been computed (and the matrix has
## not changed), then it will return the inverse from cache.


cacheSolve <- function(x, ...) {
  
  invmatrix <- x$getinv()
  
  if(!is.null(invmatrix)) {
    message("getting cached data")
    return(invmatrix)
  }
  
  data <- x$get()
  invmatrix <- solve(data, ...)
  
  x$setinv(invmatrix)
  invmatrix
  
}