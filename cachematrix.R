### This package provides a cachable interface for inversed matrices.
### Includes two functions, one to create a cachable matrix and another to 
### inverse a matrix, if not inversed previously.

## makeCacheMatrix
##
## Creates a cachable matrix object with getters and setters 
## for both the matrix and its inverse.
## 
## @param x matrix
## 
## @return list(set, get, setinverse, getinverse)
makeCacheMatrix <- function(x = matrix()) {
  # This is the internal inverse cache object.
  # We set this object to the inverse of the matrix.
  inversed <- NULL
  
  # Sets current matrix object
  #
  # @param y matrix
  set <- function(y) {
    x <<- y
    inversed <<- NULL
  }
  
  # Gets current matrix object
  #
  # @return matrix
  get <- function() x
  
  # Sets the inverse of the current matrix object.
  # This will be used as a caching interface.
  #
  # @param inv matrix
  setinverse <- function(inv) inversed <<- inv
  # Gets the cached inversion of the current matrix object
  #
  # @return matrix Inversed matrix
  getinverse <- function() inversed
  
  # And finally make the internal functions public/accessible
  # by returning them in a list.
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve
##
## Generates an inversed matrix from a makeCacheMatrix object.
## If inversion was already computed previously, this returns the cached
## result, skipping an extra computation.
##
## @warning         If the matrix (created with makeCacheMatrix) has changed,
##                  this will re-calculate the inversion.
## 
## @param x list    makeCacheMatrix object
## 
## @return matrix   Inversed matrix
cacheSolve <- function(x, ...) {
  # Get inversed matrix of the makeCacheMatrix object.
  # This can be NULL initially or when matrix has changed inside the object created via makeCacheMatrix.
  # When NULL, we will recalculate the inversion.
  inversed <- x$getinverse()
  
  # If the object does have an inversed matrix value, let's return it immediately.
  if(!is.null(inversed)) {
    message("getting cached inversed matrix")
    return(inversed)
  }
  
  # Apparently we haven't inversed the matrix yet or the matrix was changed.
  # Let's calculate its inversion and cache it in the object created via makeCacheMatrix().
  data <- x$get()
  # Calculate the inversion.
  inversed <- solve(data, ...)
  # Cache the inversed matrix.
  x$setinverse(inversed)
  
  # Return inversed matrix.
  inversed
}
