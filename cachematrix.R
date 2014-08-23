## The makeCacheMatrix function and cacheSolve function below
## can be used together to cache the inverse of a matrix within
## a special object. If present, the inverse can be retrieved
## from the cache. If the inverse of a particular matrix is not
## in the cache, then it can be calculated and then itself be 
## cached.


## This makeCacheMatrix function will make a special object that 
## can be used to store the inverse of a matrix.

makeCacheMatrix <- function(x = matrix()) {
  v <- NULL
  setmatrix <- function(z) {
    x <<- z
    v <<- NULL
  }
  getmatrix <- function() x 
  setinverse <- function(inverse) v <<- inverse
  getinverse <- function() v
  list (setmatrix = setmatrix, getmatrix = getmatrix, 
        setinverse = setinverse, 
        getinverse = getinverse)
}


## This cacheSolve function can be used to retrieve a cached inverse
## if it has already been computed. Otherwise, the function will
## calculate the inverse and then cache it.

cacheSolve <- function(x, ...) {
  v <- x$getinverse()
  if(!is.null(v)) {
    message("getting the cached data")
    return(v)
  }
  matrixdata <- x$getmatrix()
  v <- solve(matrixdata, ...)
  x$setinverse(v)
  v
}