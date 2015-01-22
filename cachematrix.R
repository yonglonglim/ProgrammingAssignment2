## The first functionsets the value of the matrix and its inverse
## The second function returns the value of the inverse matrix.
## If it has already been calculated, the second function retrives the value from the cache.

## This function creates a list containing a function to 
## set the value of the matrix 
## get the value of the matrix
## set the value of the inverse matrix
## get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## CacheSolve returns a matrix that is the inverse of 'x'. 
## If the inverse has already been calculated, it gets the inverse matrix from the cache and skips the computation.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
v<-makeCacheMatrix()
v$set( matrix(1:4, 2, 2))
v$get()
cacheSolve(v)
