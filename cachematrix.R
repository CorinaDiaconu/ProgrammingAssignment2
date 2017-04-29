## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(m = matrix()) {
  ## set the value of the matrix
  inv <- NULL
  set <- function(y) {
    m <<- y
    inv <<- NULL
  }
  
  ## get the value of the matrix
  get <- function() m
  ## set the inverse matrix
  setInverse <- function(inverse) inv <<- inverse
  ## get the inverse matrix
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(m, ...) {
  ## Return a matrix that is the inverse of m
  inv <- m$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- m$get()
  inv <- solve(mat, ...)
  m$setInverse(inv)
  inv
}