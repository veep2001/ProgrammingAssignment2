## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix<- function(x = numeric()) {
  inv <- NULL
  set <- function(A) {
    x <<- A
    inv <<- NULL
  }
  get <- function() {
    x
  }
  setinv <- function(I) {
    inv <<- I
  }
  getinv <- function() {
    inv
  }
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
