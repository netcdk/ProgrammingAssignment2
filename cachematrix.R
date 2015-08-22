## "Caching the Inverse of a Matrix"
## "R Programming" - Assignment 2 - by CDK
## The following pair of functions cache the inverse of a matrix.

## This first function creates a special "matrix" object that can
## be used to cache its inverse. This special "matrix" object 
## contains four items - set, get, setSolve, getSolve. Note that 
## the inverse is not calculated here; rather, this is preparing
## the appropriate inputs for the second function.

makeCacheMatrix <- function(x = matrix()) {
  ## Establish local "s" (think: "solved") variable with NULL
  ## value.
  s <- NULL
  
  ## Now we start creating the four items within this special
  ## "matrix" object.
  
  ## 1: Used to set the value of the given matrix.
  set <- function(y) {
    ## Set into variables in parent environment.
    x <<- y
    s <<- NULL
  }
  ## 2: Used to get the value of the given matrix.
  get <- function() x
  ## 3: Used to set the value of inverse of the given matrix.
  setsolve <- function(solve) s <<- solve
  ## 4: Used to get the value of the given matrix.
  getsolve <- function() s
  
  ## Bundle together the special "matrix" object.
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

## Now we take the special "matrix" object, and solve!
## This second function computes the inverse of the special "matrix"
## object that was returned by the first function (makeCacheMatrix).
## If the inverse has already been calculated (and the matrix has
## not changed), then this function will retrieve/return the inverse 
## from the cache. If the inverse has not yet been calculated, then
## the inverse will be caluclated, added to the cache, and returned.

cacheSolve <- function(x, ...) {
  ## Pull whatever existing "inverse" value is available, if any,
  ## from special "matrix" object.
  s <- x$getsolve()
  ## Check to see if this existing "getsolve" value is not NULL,
  ## and, if TRUE, return the value.
  if(!is.null(s)) {
    message("getting cached data")
    ## Return the (cached) inverse of the given matrix.
    return(s)
  }
  ## Get the value of the given matrix.
  data <- x$get()
  ## Solve for inverse of the given matrix.
  s <- solve(data, ...)
  ## Setting "inverse" value into cache.
  x$setsolve(s)
  ## Return the inverse of the given matrix.
  s
}