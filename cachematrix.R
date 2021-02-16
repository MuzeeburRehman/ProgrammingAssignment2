## This experiment is for a pair of functions to be written, namely,
## "makeCacheMatrix" and "cacheSolve" to cache the matrix inverse.

## MakeCacheMatrix is a function that generates a special "matrix" 
##object capable of creating a particular "matrix" object.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
 	 x <<- y
 	 m <<- NULL
  		}
  get <- function()
  x
  setInverse <- function(inverse)
  m <<- inverse
  getInverse <- function()
  m 
  list(set = set, get = get, 
  setInverse = setInverse, 
  getInverse = getInverse)


}


## The inverse cache for the input cache
##which is an invertible square matrix

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if(!is.null(m)){
  	message("getting cached data")
  	return(m)
  		}
  mat <- x$get() 
  m <- solve(mat,...)
  x$setInverse(m)
  m
}
matrix1 <- matrix(rnorm(9),3,3)
matrix1
matrix2 <- makeCacheMatrix(matrix1)

cacheSolve(matrix2)