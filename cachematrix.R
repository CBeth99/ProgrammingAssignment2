## Programming Assignment 2
## File created:  4/23/2015
## Author: CBeth99
## This R code contains two functions:
## makeCacheMatrix & cacheSolve

## The makeCacheMatrix function creates a matrix object that caches its inverse

makeCacheMatrix <- function(x = matrix()) {
  ##Create the makeCacheMatrix function that stores 4 other functions 
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmatrix <- function(matrix) m <<- matrix
  getmatrix <- function() m
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)

}


## The cacheSolve function computes the inverse of the matrix returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getmatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setmatrix(m)
  m
}

