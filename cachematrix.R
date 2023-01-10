## Put comments here that give an overall description of what your
## functions do

## The makeCacheMatrix function creates a matrix object that caches its inverse making it an invertible square matrix.

makeCacheMatrix <- function(x = matrix()) {          ## Defines the argument as a matrix
  inv <- NULL                                        ## Creates inv as the variable set to default as NULL (matrix inverse value)
  set <- function(y) {                               ## Define the set function to change cached values of a matrix in
    x <<- y                                          ## parent environment
    inv <<- NULL                                     ## In case of new matrix, resets inv to NULL
  }
  get <- function() x                                ## Get function returns the value of the matrix arguments
  setInverse <- function(inverse) inv <<- inverse    ## <<- operator assigns value of inv in parent environment
  getInverse <- function() inv                       ## Gets the value of inv where called
  list(set = set, get = get,                         
       setInverse = setInverse, getInverse = getInverse)    ## Required for $ operator functions to reference
}

##The cacheSolve function calculates the inverse of the "matrix" returned by makeCacheMatrix function defined above.
## If the inverse has been calculated with no changes to the matrix cacheSolve returns the inverse from the cache.

cacheSolve <- function(x, ...) {                           
        ## Return a matrix that is the inverse of 'x'
      inv <- x$get.inverse()                          ##Returns matrix that is the inverse of X
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$set.inverse(inv)
  inv
}
