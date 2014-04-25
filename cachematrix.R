## cachematrix.R

## makeCacheMatrix
## This fucntion creates a list of vectors where each vector is its own function.
## After running this function, the matrix is set. 
## In addition, four additional functions can be run including
## 1. get() = gets the value of the matrix; 2. getInverse() = gets the inverse of the matrix
## 2. set() = sets new values for the matrix; 4. setInverse() = sets inverse of matrix.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) i <<- solve
  getInverse <- function() i
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function will test if the inverse of the matrix has already been cached. 
## If not, it will find the inverse and cache and return the inverse matrix. 
## If it already has been cached, it will return the inverse matrix.

cacheSolve <- function(x, ...) {
  i <- x$getInverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setInverse(i)
  i
}
