
## The purpose of this function is to invert a matrix and
## cache its result.

makeCacheMatrix <- function(x = matrix()) {
  
  ## Initialize the inv to NULL
  
  inv <- NULL
  
  ## assign inputs to cache matrix and NULL the inverse
  
  set <- function(y) {
    
    x <<- y
    
    inv <<- NULL
    
  }
  
  ## assign functions to access cache
  
  get <- function() x
  
  setinverse <- function(inverse) inv <<- inverse
  
  getinverse <- function() inv
  
  ## create list and return said list to parent environment
  
  list(set = set, get = get,
       
       setinverse = setinverse,
       
       getinverse = getinverse)
  
}



## cacheSolve() inverts a matrix, or retrieves from cache if it already exists



cacheSolve <- function(x, ...) {
  
  ## attempt to retrieve matrix from cach
  
  invMatrix <- x$getinverse()
  
  if(!is.null(invMatrix)) {
    
    message("getting cached data")
    
    return(invMatrix)
    
  }
  
  ## if cache is empty, invert the matrix, set the cache, and return
  
  data <- x$get()
  
  invMatrix <- solve(data, ...)
  
  x$setinverse(invMatrix)
  
  invMatrix
  
  ## Return a matrix that is the inverse of 'x'
  
}
