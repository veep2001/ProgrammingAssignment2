## This file as two functions. The first, makeCacheMatrix(), creates a function 
## environment that can create, access, and store a Matrix's solution, or inverse.
## The second fucntion, cacheSolve(), will access an object that is assigned from 
## the first fucntion to solve for its inverse and return that to the console, if 
## the inverse has not already been cached. 

## This function is designed to create a "matrix" object. Assigning this function 
## to an object will allow one to store a matrix, recall the matrix, store its
## inverse, and recall its inverse. It takes advantage of lexical scoping by 
## storing the inverse of the function in the function environment. The subfunctions
## can then cache their output in the parent environment so that they can be 
## recalled as needed.

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


## This second function when paired with the first allows one to acccess
## the objects stored within the function environment of makeCacheMatrix()
## The specific purpose of this function is to test if the inverse of the 
## matrix stored in the object created with function one has been stored.
## If it is stored already, then this function calls the inverse directly.
## If it has not been solved for already, then it solves the stored matrix
## directly, stores the inverse in the function environoment of the object,
## and then reports the inverse to the console. 

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
