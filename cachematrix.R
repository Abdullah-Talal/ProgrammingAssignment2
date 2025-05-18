## Creates a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  ## Set a new matrix and reset cached inverse
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  ## Get the matrix
  get <- function() x
  
  ## Cache the inverse
  setinverse <- function(inverse) inv <<- inverse
  
  ## Get the cached inverse
  getinverse <- function() inv
  
  ## Return a list of functions
  list(set = set, 
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Computes the inverse or retrieves it from cache
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  
  ## Return cached inverse if available
  if (!is.null(inv)) {
    message("getting cached inverse")
    return(inv)
  }
  
  ## Compute inverse and cache it
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
