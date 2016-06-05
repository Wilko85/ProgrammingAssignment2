## Caching the Inverse of a Matrix:
## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## Below are two functions that are used to create an object that 
## stores a matrix and caches its inverse.

## This function creates a matrix object that can cache its inverse.

makeCacheMatrix <- function(b = matrix()) {
  inv <- NULL
  set <- function(a) {
    b <<- a
    inv <<- NULL
  }
  get <- function() b
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function computes the inverse of the matrix created by 
## makeCacheMatrix. If the inverse has already been calculated (and the 
## matrix has not changed) then it should retrieve the inverse from the cache.

cacheSolve <- function(b, ...) {
  ## Return a matrix that is the inverse of 'b'
  inv <- b$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- b$get()
  inv <- solve(mat, ...)
  b$setInverse(inv)
  inv
}