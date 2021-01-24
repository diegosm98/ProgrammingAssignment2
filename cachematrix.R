##functions that can cache the inverse of a matrix
##to get the result first assign to a variable  the makeCacheMatrix with the matrix as its only argument 
##then use the cache solve function with the variable used above
##As the assignment stated, this code assumes the matrix provided is a square invertible matrix and will not work otherwise.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(solveMatrix) inv <<- solveMatrix
  getInverse <- function() inv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)){
   
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setInverse(inv)
  inv      
}