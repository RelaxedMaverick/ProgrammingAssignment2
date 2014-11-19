## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  ## initialize the inverse copy to null
  inverse <- NULL
  ## Setter for the matrix
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  ## Getting for the matrix
  get <- function() x
  ## Setter for caching the matrix inverse
  setinverse <- function(inv) inverse <<- inv
  ## Getter for retreiving the matrix inverse from cached copy
  getinverse <- function() inverse
  ## Creating the list containing functions 
  list(set = set, 
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## check if we can get the cached inverse copy
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    ## Returning cached inverse copy
    return(inv)
  }
  data <- x$get()
  ## Compute matrix inverse
  inv <- solve(data)
  ## Cache the inverse matix
  x$setinverse(inv)
  ## Return a matrix that is the inverse of 'x'
  inv
}
