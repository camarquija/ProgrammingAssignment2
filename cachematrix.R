## For this assignment I used the example functions to
## create the special matrix and then calculate its inverse

## For the special matrix creation I followed the fourth
## steps in the example
## 1. set the value of the matrix
## 2. get the value of the vector
## 3. set the value of the inverse matrix
## 4. get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function returns the value of the inverse matrix
## First it looks for a chached inverse matrix value and
## returns it. If there is not cached value, then it calculates
## the inverse matrix and gives the value.

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
