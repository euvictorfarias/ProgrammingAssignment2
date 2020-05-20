## Data Science - Coursera
## Course 2 - Week 3
## Programming Assignment 2
## Author: Victor Farias


## Two functions created to define, create, store and/or show a matrix and it's inverse.


## makeCacheMatrix defines a matrix and it's inverse, if setInverse is called.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(matriz) m <<- solve(matriz)
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve verifies if the Inverse exists, if yes them it is showed, otherwise, it is created, stored and showed.

cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  if(!is.null(m)) {
    message("Getting cached data...")
    return(m)
  } else{
    message("Creating data...")
    data <- x$get()
    m <- solve(data)
    x$setInverse(m)
    m
  }
}
