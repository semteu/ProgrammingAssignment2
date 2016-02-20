## Put comments here that give an overall description of what your
## functions do

## The function makeCacheMatrix creates a special "matrix" containing
## a set of functions to set and get data from the matrix
makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y){
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setSolve <- function (solve) s <<- solve
  getSolve <- function () s
  list(set = set, get = get, 
       setSolve = setSolve,
       getSolve = getSolve)
}


## Write a short comment describing this function
## The function cacheSolve compute the inverse of a "special" matrix
## returned by makeCacheMatrix. It retrieves the inverse from the cache
## if it has already been calculated.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  s <- x$getSolve()
  if(!is.null(s)){
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data)
  x$setSolve(s)
  s
}
