## 'makeCacheMatrix' and 'cacheSolve' are used to solve the problem of computing the inverse of a matrix
## only when necessary, caching the result for future uses.
##
## Example of use:
## m <- makeCacheMatrix(matrix(c(1.2, 3, -1.4, 7), 2, 2))
## cacheSolve(m)
## m$getinv()

## 'makeCacheMatrix' creates a special matrix object that can cache its inverse.
## The object provides 4 functions:
## get(): returns the numeric contents of the matrix.
## getinv(): returns the value of the inverse computed so far. If it  was never computed, returns NULL.
## set(x): updates the matrix to be 'x'. Resets the inverse to NULL.
## setinv(x): updates the inverse of the matrix to be 'x'. Should be used only by 'cacheSolve'.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## 'cacheSolve' computes the inverse of the special matrix returned by 'makeCacheMatrix'
##  If the inverse has already been calculated (and the matrix has not changed), 
##  then retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    # getting cached data
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
