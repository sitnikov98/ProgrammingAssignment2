## Put comments here that give an overall description of what your
## functions do
	
## makeCacheMatrix: This function can calculate and give us the inverse of inputting matrix, also it can cache it

makeCacheMatrix <- function(x = matrix()) {
  n <- NULL
  set <- function(y){
  x <<- y
  n <<- NULL
  }
  get <- function()x
  setInverse <- function(inverse) n <<- inverse
  getInverse <- function() n 
  list(set = set, get = get, 
  setInverse = setInverse, 
  getInverse = getInverse)
}


## cacheSolve: this function returns the inverse matrix returned by makeCacheMatrix.
## When the inverse has already been calculated (and the matrix has not changed),
## the caching log should extract the inverse from the cache.
## To do this, suppose that the matrix is always invertible.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  n <- x$getInverse()
  if(!is.null(n)){
  message("getting cached data")
  return(n)
  }
  mat <- x$get()
  n <- solve(mat,...)
  x$setInverse(n)
  n
}
