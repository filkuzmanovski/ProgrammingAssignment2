## These functions will cache the inverse of a matrix


## Created a matrix object that will cache its inverse
makeCacheMatrix <- function( m = matrix() ) {
  
  ## Inverse property initialized
  i <- NULL
  
  ## Method to set the matrix
  set <- function( matrix ) {
    m <<- matrix
    i <<- NULL
  }
  
  ## Method to obtain the matrix
  get <- function() {
    ## Return the matrix
    m
  }
  
  ## Method for setting the inverse of the matrix
  setInverse <- function(inverse) {
    i <<- inverse
  }
  
  ## Method to get the inverse of the matrix
  getInverse <- function() {
    ## Returning the inverse property
    i
  }
  
  ## Returning a list of the methods
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Compute the inverse of the special matrix returned by "makeCacheMatrix".
## If the inverse has already been calculated then cachesolve will retrieve the
## inverse from the cache.
cacheSolve <- function(x, ...) {
  
  ## Return the matrix that is the inverse of 'x'
  m <- x$getInverse()
  
  ## Return the inverse only if its already set
  if( !is.null(m) ) {
    message("getting cached data")
    return(m)
  }
  
  ## Get matrix from our object
  data <- x$get()
  
  ## Calculate inverse using matrix multiplication
  m <- solve(data) %*% data
  
  ## Set inverse to the object
  x$setInverse(m)
  
  ## Return matrix
  m
}