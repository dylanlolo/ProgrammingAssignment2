## Matrix inversion is usually a costly computation, it is beneficial to caching 
## the inverse of a matrix rather than compute it repeatedly, the two functions 
## created below would help to solve this problem and make the attempt to inverse 
## marrix more time effecient.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inverse <<- inverse
  getInverse <- function() inverse
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}



## This function computes the inverse of the special "matrix" returned by the
## makeCacheMatrix that has been created above. If the inverse has already been  
## computed (and the matrix has not changed), then the cachesolve should retrieve
## the inverse from the cache.

cacheSolve <- function(x, ...) {
  inverse <- x$getInverse()
  if (!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inverse)
  inverse
}



