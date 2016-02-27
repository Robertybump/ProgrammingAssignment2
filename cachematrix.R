## Coursera R Programming Assignment 2
## Matrix inversion is usually a costly computation and there may be some benefit to 
## caching the inverse of a matrix rather than compute it repeatedly (there are also 
## alternatives to matrix inversion that we will not discuss here). Your assignment is 
## to write a pair of functions that cache the inverse of a matrix.
## Write the following functions:

## makeCacheMatrix: This function creates a special "matrix" object that can cache its 
## inverse.
## cacheSolve: This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the matrix has 
## not changed), then the cachesolve should retrieve the inverse from the cache.
## Computing the inverse of a square matrix can be done with the solve function in R. 
## For example, if X is a square invertible matrix, then solve(X) returns its inverse.

## For this assignment, assume that the matrix supplied is always invertible.

## The following functions below create a special object to store the matrix and caches its
## inverse. 

## The following function creates the matrix object

makeCacheMatrix <- function(x = matrix()) {
  inver <- NULL
  set <- function(y) {
    x <<- y
    inver <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inver <<- inverse
  getInverse <- function() inver
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function below computes the inverse of the matrix created by the function
## makeCacheMatrix above.If the inverse has already been calculated then it should 
## retrieve the inverse from the cache created.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inver <- x$getInverse()
  if (!is.null(inver)) {
    message("getting cached data/matrix")
    return(inver)
  }
  mat <- x$get()
  inver <- solve(mat, ...)
  x$setInverse(inver)
  inver
}