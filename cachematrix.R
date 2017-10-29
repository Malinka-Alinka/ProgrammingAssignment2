## Put comments here that give an overall description of what your
## functions do

## Matrix inversion is usually a costly computation 
## and there may be some benefit to caching the inverse
## of a matrix rather then complute it repeatedly.
## Below are a pair of functions thet are used to create
## a special object that stores a matrix and cahes its inverse.

## Matrix inversion is usually a costly computation and there 
## may be some benefit to caching the inverse of a matrix rather 
## than compute it repeatedly.
## I write a pair of functions that cache the inverse of a matrix.

## This function creates a spesial "matrix" object that can 
## cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get=get, setInverse = setInverse, getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)){
    message("Get cached data")
    return(inv)
  }
  matr <- x$get()
  inv <- solve(matr, ...)
  x$setInverse(inv)
  inv
}
