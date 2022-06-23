## Put comments here that give an overall description of what your
## functions do

## Creates matrix that inverses input argument and caches it

makeCacheMatrix <- function(x = matrix()) {
 inverse <- NULL
 set <- function(y) {
   x <<- y 
   inverse <<- NULL
 }

 get <- function() x
  setInverse <- function() inverse <<- solve(x)
  getInverse <- function() inverse
  list( set = set, get = get, 
        setInverse = setInverse, 
        getInverse = getInverse)
} 
## Needed to get the inverse from makeCacheMatrix/add to parent environment
  cacheSolve <- function(x, ...)  {
    inverse <-x$getinverse()
    if(!is.null(inverse)) {
      message("getting cached data")
      return(inverse)
    }
    data <-x$get()
    inverse <- inverse(data,...)
    x$setInverse(inverse)
    inverse
  }
