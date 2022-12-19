## The following functions allow the benefit of caching the inverse of a matrix
## rather than computing the inverse of the matrix repeatedly. 

## The makeCacheMatrix creates a matrix which can cache its inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The cacheSolve computes and returns the inverse of the matrix created by makeCacheMatrix.
## If the matrix has not been changed and its inverse has already been calculated, cacheSolve recalls the inverse of the matrix from the cache.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i        ## Return a matrix that is the inverse of 'x'
}
