## This function makeCacheMatrix gives you setter getter for the matrix you pass as an argument
## Also provides the 1) setinverse to create the inverse of a matrix
## and 2) getinverse to get the inverse matrix 

## This function creates a special "matrix" object
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()){
  ## initialize inverse matrix m with NULL
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  ## get the matrix x, passed as an argument.
  get <- function() x
  
  ## set inverse of the matrix passed as an argument
  setinverse <- function(solve) m <<- solve
  
  ## get inverse of the matrix passed as an argument
  getinverse <- function() m
  
  ## list all the functions and their definition
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## get inverse of matrix 'x'
  m <- x$getinverse()
  ## if the inverse exist in cache
  if(!is.null(m)) {
    message("getting cached incverse of matrix")
    return(m)
  }
  ## if the inverse does not exists . get it . set it . return it.
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
