## The two functions below work together to precompute an inverse of a matrix efficiently through the 
## use of a caching proxy

## creates a getter and setter to a matrix which allow a proxy to provide additional functionality.  
## In thie case the extra functionality is storing a matrix 'next' to the matrix defined in the argument
## list.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmatrix <- function(matrix) m <<- matrix
  getmatrix <- function() m
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}


## finds the inverse of a matrix, caching the results of the computation through the use 
## of the proxy defined in makeCacheMatrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getmatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setmatrix(m)
  m
}
