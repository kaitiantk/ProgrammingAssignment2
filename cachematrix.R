
# This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  ivr <- NULL
  set <- function(y) {
    x <<- y
    ivr <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) ivr <<- inverse
  getinverse <- function() ivr
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


#This function computes the inverse of the special "matrix" returned 
#by makeCacheMatrix above. If the inverse has already been calculated 
#(and the matrix has not changed), then the cachesolve should retrieve the 
#inverse from the cache
cacheSolve <- function(x, ...) {
  ivr <- x$getinverse()
  if(!is.null(ivr)) {
    message("getting cached data")
    return(ivr)
  }
  data <- x$get()
  ivr <- solve(data, ...)
  x$setinverse(ivr)
  ivr
}
