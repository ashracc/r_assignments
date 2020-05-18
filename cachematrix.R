## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
makeCacheMatrix <- function(x = matrix()) {
  ## This function will receive x, which is a square invertible matrix
  ## It will create an object that can cache its inverse
  ## It will return a list of functions which are: 
  ## set_mtx()    set the matrix
  ## get_mtx()    get the matrix
  ## set_inv()    set the inverse
  ## get_inv()    get the inverse
  inv = NULL
  set_mtx = function(y) {
    x <<- y
    inv <<- NULL
  }
  get_mtx = function() x
  set_inv = function(inverse) inv <<- inverse 
  get_inv = function() inv
  list(set_mtx=set_mtx, get_mtx=get_mtx, set_inv=set_inv, get_inv=get_inv)
}


cacheSolve <- function(x, ...) {
  ## x is the output of makeCacheMatrix()
  ## It will return the inverse of the original matrix input to makeCacheMatrix()
  inv = x$get_inv()
  
  # if the inverse has already been calculated
  if (!is.null(inv)){
    # get it from the cache and skips the computation. 
    message("Already cached")
    return(inv)
  }
  
  # otherwise, computes the inverse
  message("Computing inverse")
  mat_data = x$get_mtx()
  inv = solve(mat_data, ...)
  
  # sets the value of the inverse in the cache.
  x$set_inv(inv)
  return(inv)
}
