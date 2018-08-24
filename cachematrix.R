# Caching the Inverse of a Matrix -
# Matrix inversion is usually a costly computation and there may be some 
# benefit to caching the inverse of a matrix rather than compute it repeatedly.
# The given below are a pair of functions that are used to cache the inverse of a matrix.

# The makeCacheMatrix function creates a special "matrix" object that can cache its 
# inverse.
makeCacheMatrix <- function(a = matrix()) {
  b <- NULL
  
  set <- function(c) {
    a <<- c
    
    b <<- NULL
  }
  
  get <- function() a
  
  setInverse <- function(inverse) b <<- inverse
  
  getInverse <- function() b
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}



# The cacheSolve function computes the inverse of the special "matrix" returned by 
# makeCacheMatrix above. If the inverse has already been calculated (and the matrix 
# has not changed), then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(m, ...) {
  
  # Return a matrix that is the inverse of 'm'
  inverseMatrix <- m$getInverse()
  
  if(!is.null(inverseMatrix)) {
    return(inverseMatrix)
  }
  
  data <- m$get()
  
  inverseMatrix <- solve(data, ...)
  
  m$setInverse(inverseMatrix)
  
  inverseMatrix
}

