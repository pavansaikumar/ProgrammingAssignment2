## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  Inverse <- NULL ## Setting the value to null
  set <- function(y){ ## To set the value of X using the set function
       x <<- y
       Inverse <<- NULL
  }
  
  get <- function() x ## Returns the value of Inverse
  setInverse <- function(solve) Inverse <<- solve ## Find the Inverse of the given Matrix and store it
  getInverse <- function() Inverse ## get the value of the Inverse matrix
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}


## Write a short comment describing this function
## Return the cache of the matix if available already
cacheSolve <- function(x, ...) {
  Inverse <- x$getInverse()  ## get the Inverse of the matirx if available
  if(!is.null(Inverse)){## check if the inverse is alredy calculated
    message("getting the cached data")
    return(Inverse)
  }
  data <- x$get()
  Inverse <- solve(data, ...)
  x$setInverse(Inverse)      ## Calls the set inverse function to find the inverse and set the value
  Inverse
        ## Return a matrix that is the inverse of 'x'
}
