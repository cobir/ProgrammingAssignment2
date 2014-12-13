## This file contains two functions for the creation of an object
## that stores a matrix and caches its inverse so that every time
## the invesrse of the matrix is needed it uses the cached one instead 
## of recalculating

## Create an object to hold the matrix and its inverse
makeCacheMatrix <- function(mat = matrix()) {
  solv <- NULL
  set <- function(y) {
    mat <<- y
    solv <<- NULL
  }
  get <- function() mat
  setsol <- function(sol) solv <<- sol
  getsol <- function() solv
  list(set = set, get = get,
       setsol = setsol,
       getsol = getsol)
}


# get the inverse of the matrix using the cached value 
# if one exists
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  s <- x$getsol()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  themat <- x$get()
  s <- solve(themat, ...)
  x$setsol(s)
  s
}
