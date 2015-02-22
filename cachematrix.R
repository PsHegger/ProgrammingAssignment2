## Functions for caching the inverse of matrices

## Creates a cache matrix which is able to cache it's inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(i) inv <<- i
  getinv <- function() inv
  
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Returns the inverse of a matrix, either by computing and caching it,
## or if the cache is available returns that without re-computing it
cacheSolve <- function(x, ...) {
  i <- x$getinv()
  if (!is.null(i)) {
    return(i)
  }
  
  m <- x$get()
  i <- solve(m)
  x$setinv(i)
  
  i
}
