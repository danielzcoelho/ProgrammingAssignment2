##  The purpose of theses 2 functions combined is to inverse a given matrix
##  and store the result in the computer's RAM memory ( cache ), because
##  in case of a multiple use it is not required to compute it over and over
##  again. It will be possible just to get the inverted matrix from cache.

##  makeCacheMatrix initializes 2 objects (x and invMatrix), and stores 4 functions:
##    1 - set():
##        Assign the input argument (value of the matrix) to the x object and
##        clears the value of invMatrix that had been cached by a prior execution.
##
##    2 - get():
##        Get the value of the matrix.
##
##    3 - setInvMatrix():
##        Assign the value of the inverse matrix.
##
##    4 - getInvMatrix():
##        Get the value of the inverse matrix.
##
##  Finally, the last section assigns each of these functions as a named element
##  within a list(), so it is possible to access the functions using the $ operator.

makeCacheMatrix <- function(x = matrix()) {
  invMatrix <- NULL
  set <- function(y) {
    x <<- y
    invMatrix <<- NULL
  }
  get <- function() x
  setInvMatrix <- function(inverse) invMatrix <<- inverse
  getInvMatrix <- function() invMatrix
  list(set = set, get = get, setInvMatrix = setInvMatrix, getInvMatrix = getInvMatrix)
}


##  cacheSolve calculates the inverse matrix and/or retrieve it from cache.
##  First, it calls getInvMatrix() to try to retrieve the inverse matrix. If the
##  result is not equal NULL, there is a valid, cached inverse matrix to be returned.
##  If the result is NULL, then cacheSolve gets the matrix from the input object,
##  calculates its inverse, assigns it to cache and returns it.

cacheSolve <- function(x, ...) {
  invMatrix <- x$getInvMatrix()
  if(!is.null(invMatrix)) {
    message("getting cached data")
    return(invMatrix)
  }
  data <- x$get()
  invMatrix <- solve(data, ...)
  x$setInvMatrix(invMatrix)
  invMatrix
}
