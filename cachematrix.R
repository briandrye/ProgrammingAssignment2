## two functions that can be used together to cache a matrix inverse

## a "matrix" function that contains other functions for setting/getting
## the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  inverse_cached <- NULL
  set <- function(y) {
    x <<- y
    inverse_cached <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inverse_cached <<- inverse
  getinverse <- function() inverse_cached
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve operates on a special matrix created by makeCacheMatrix()
## if the inverse has already been calculated, it is returned. 
## otherwise, it is caculated and stored.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}

