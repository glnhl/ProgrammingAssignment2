## The following functions allow the benefit of caching the inverse of a matrix
## rather than computing the inverse of the matrix repeatedly. 

## The makeCacheMatrix creates a matrix which can cache its inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL                        #inverse of the matrix 'x'
  set <- function(y) {             #set the value of the matrix
    x <<- y                        #assign value y to the matrix 'x'
    i <<- NULL                     #reset the inverse of 'x' to NULL
  }
  get <- function() x              #get the value of the matrix 'x'
  setinverse <- function(solve) i <<- solve        #set the inverse of the matrix
  getinverse <- function() i       #get the inverse of the matrix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)    #list the functions
}


## The cacheSolve computes and returns the inverse of the matrix created by makeCacheMatrix.
## If the matrix has not been changed and its inverse has already been calculated, cacheSolve recalls the inverse of the matrix from the cache.

cacheSolve <- function(x, ...) {   #compute and return the inverse of the matrix
  i <- x$getinverse()              #get the inverse matrix from the cache
  if(!is.null(i)) {                #if the inverse of the matrix has already been calculated (is not null)
    message("getting cached data") #return the message "getting cached data"
    return(i)                      #return the inverse matrix from the cache
  }                               
  data <- x$get()                  #get the data from the matrix
  i <- solve(data, ...)            #calculate the inverse of the matrix
  x$setinverse(i)                  #set the inverse in the cache
  i                                ##Return the inverse of the matrix 'x'
}
