## The following functions aim at caching a matrix and its inverse
## computation and storage. In order to use these functions you need:

## 1 - assign the first function makeCacheMatrix to an object you created. 
## 2 - you just need to use the set method to assign any matrix 
##     you are working with, 
## 3 - to get its inverse, you may calculate a priori 
##     and assing it  with "setinverse" method or you may call cacheSolve


## The following function builds the cached matrix and its
## associated methods of setting, getting the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  inverseOfx <- NULL
  set <- function(y) {
    x <<- y
    inverseOfx <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inverseOfx <<- inverse
  getinverse <- function() inverseOfx
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## Write a short comment describing this function
## The following function aims at solving the inverse 
## matrix cached. 

cacheSolve <- function(chachedMatrix, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverseOfx <- chachedMatrix$getinverse()
  if(!is.null(inverseOfx)) {
    message("getting cached data")
    return(inverseOfx)
  }
  data <- chachedMatrix$get()
  inverseOfx <- solve(data, ...)
  chachedMatrix$setinverse(inverseOfx)
  inverseOfx
}
