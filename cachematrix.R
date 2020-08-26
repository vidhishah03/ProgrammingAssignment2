

## Making a Matrix to Cache its objects

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
  x <<- y
  inv <<- NULL
  }
  get <- function() {x}
  setInverse <- function(inverse) {inv <<- inverse}
  getInverse <- function() {inv}
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Returns inverse of matrix returned by the makeCacheMatrix function

cacheSolve <- function(x, ...) {
  inv <- x$getmean()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}
