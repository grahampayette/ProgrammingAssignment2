## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## initializes set/get functions for a square matrix

makeCacheMatrix <- function(x = matrix()) {
  ## initialize variables
  
  tmp <- NULL
  
  ## test for type
  if (!is.matrix(x)) {
    warning("argument is not a matrix: returning NA")
    return(NA_real_)
  }
  
  ## set value
  set <- function(y) {
    x <<- y
    tmp <<- NULL
  }  
  
  ## get value
  get <- function() x 
  ## get/set stored values
  setinv <- function(solve) tmp <<- solve
  getinv <- function() tmp 
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)    
  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  tmp <- x$getinv()
  
  if(!is.null(tmp)) {
    message("getting cached data")
    return(tmp)
  }
  data <- x$get()
  tmp <- solve(data, ...)
  x$setinv(tmp)
  tmp 
  
}
