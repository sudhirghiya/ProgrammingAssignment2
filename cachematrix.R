## Caching the Inverse of a Matrix:
## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## Below are a pair of functions that are used to create a special object that 
## stores a matrix and caches its inverse.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
## @x: a square invertible matrix
## return: a list containing functions to
##         1. set function to set the matrix
##         2. get function to get the matrix 
##         3. setInverse function to set the inverse 
##         4. getInverse function to get the inverse
##         5. list function to list functions
  
  inv <- NULL
  set <- function(y) {
    # use `<<-` "operator" to assign value to an non-current object     
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" created by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x', where 'x' in input to cachesolve
  inv <- x$getInverse()
  if (!is.null(inv))        # if the inverse is already calculated
  { 
    message("getting cached data")  
    return(inv)             # get cached inverse & skip all computations & exit.
  }
  
  # if not cached calculates the inverse 
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}