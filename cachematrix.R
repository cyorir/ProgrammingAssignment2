##  makeCacheMatrix will return a set of functions for manipulating a
##    an invertible square matrix that caches its inverse
##  cacheSolve will update the inverse of that matrix if necessary and
##    return the inverse

##  the list returned by makeCacheMatrix has 4 member functions :
##    $set(y) will update the matrix to y
##    $get() will return the matrix
##    $setinv() will update the cached inverse
##    $getinv() will return the cached inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function() inv <<- solve(x)
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


##  cacheSolve will return the inverse of a matrix created by makeCacheMatrix
##  the input should be the list of functions returned by makeCacheMatrix
##  the return value is the inverse of the matrix set by makeCacheMatrix or
##    set by the function $set() returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
    invx <- x$getinv()
    if (!is.null(invx)) {
      message("getting cached inverse")
      return(invx)
    } else {
      x$setinv()
      return(x$getinv())
    }
}
