## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inverseValue <- matrix() ## Set up inverseValue as a matrix
  inverseValue <- NULL ## Set inverseValue to NULL
  
  ## Set the value of the matrix x
  set <- function(y) {
    x <<- y ## Assign a value to matrix x in parent environment equal to y
    inverseValue <<- NULL ## Assign NULL to the inverseValue matrix in the parent environment
  }
  
  ## Get the value of the matrix x
  get <- function() x 
  
  ## Set the inverse of the matrix x
  setInverse <- function(solve) inverseValue <<- solve
  
  ## Get the value of the inverse of the matrix x
  getInverse <- function() inverseValue
  
  ## Set up a list with all of these elements
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  ## Return a matrix that is the inverse of 'x'
  inverseValue <- x$getInverse() ## Load
  
  if(!is.null(inverseValue)){
    message("Getting cached data")
    return(inverseValue)
  }
  data <- x$get()
  inverseValue <- solve(data,...)
  x$setInverse(inverseValue)
  inverseValue
}
