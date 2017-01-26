# Functions listed below provide a way to minimise time spent on getting inverse of
# matrix by caching it. For this purpose we construct a special data structure using 
# makeCacheMatrix() to store the matrix and its inverse.
# The cacheSolve() function returns the inverse of the special matrix object provided to it.
# It does so by using the solve() function in case it does not find a pre-computed non-stale
# inverse.

## Constructs a special matrix like object which stores the matrix along with setter/getter
## member functions.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) {
    inv <<- inverse
    }
  getinverse <- function() inv
  list(set = set, 
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## Returns the inverse of matrix object of the type returned by makeCacheMatrix.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached inverse")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  
  inv
}
