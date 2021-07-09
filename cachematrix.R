## Put comments here that give an overall portrayal of what your 
## functions do 
## Our point in this test is to type in a match of capacities, namely, 
## "makeCacheMatrix" and "cacheSolve" that cache the inverse of a matrix 
## Write a brief comment depicting this function 
## makeCacheMatrix may be a work which makes a special "framework" protest that can 
## cache its inverse for the input (which is an invertible square matrix)
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}
## Compose a brief comment depicting this function 
## cacheSolve may be a work which computes the inverse of the special "matrix" 
## returned by makeCacheMatrix over. In the event that the converse has as of now been calculated 
## (and the matrix has not changed), at that point the cachesolve ought to recover the 
## inverse from the cache
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached result")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}
## Caching the Inverse of a Matrix: 
## Matrix inversion is ordinarily a costly computation and there may be some 
## benefit to caching the reverse of a matrix instead of compute it repeatedly. 
## Below are a match of functions that are utilized to form a extraordinary object that 
## stores a matrix and caches its inverse. 
## This work makes a special "matrix" protest that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}
## This work computes the converse of the special "matrix" made by 
## makeCacheMatrix over. On the off chance that the inverse has as of now been calculated (and the 
## matrix has not changed), at that point it ought to recover the inverse from the cache.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}
