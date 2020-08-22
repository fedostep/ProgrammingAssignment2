## This functions pretend to reduce the computational cost of computing
## repeatedly the inverse of a matrix. Instead, these functions allow 
## the process take place in two steps. 

makeCacheMatrix <- function(x = matrix()) {
  ## Firstly, the makeCacheMatrix function pretends to create a matrix, also
  ## calculated and stores the inverse of that matrix lization of the variable that will store the inverse matrix 
  j <- NULL
  #The set function assign the the values from the parent environment
  #If there is already a matrix in matr it will clear it, in order 
  #recalculate the inverse.This is done when the initial matrix is change.
  set <- function(y){
    x <<- y
    j <<- NULL
  }
  get <- function()x
  setInverse <- function(inverse) j <<- inverse
  getInverse <- function() j 
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}
## Firstly, the cacheSolve function pretends to create a matrix, also
## calculated and stores the inverse of the matrix of the MakeCacheMatrix
## function. The function has an object as main argument

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  j <- x$getInverse()
  if(!is.null(j)){
    message("getting cached data")
    return(j)
  }
  mat <- x$get()
  j <- solve(mat,...)
  x$setInverse(j)
  j
}