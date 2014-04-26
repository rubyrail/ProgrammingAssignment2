# Matrix inversion is usually a costly computation and there may be some
# benefit to caching the inverse of a matrix rather than computing it
# repeatedly. The following pair of functions are written to cache 
# the inverse of a matrix.

# In R, the function solve(X) returns X's inverse
# where X is a square invertible matrix.
# So saying solve is equivalent to saying inverse of matrix.
# For the following pair of functions, the matrix supplied
# is assumed to be invertible.

# `makeCacheMatrix`: This function creates a special "matrix" 
# object that can cache its inverse.
# The special "matrix" object is actually a list of functions to
# 1. set the value of the "matrix"
# 2. get the value of the "matrix"
# 3. set the value of the inverse
# 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  # s is a variable representing the inverse of matrix x
  # s is NULL before any calculation for the inverse
  s <- NULL
  
  # set() seems to be useless
  # but keep it here just in case I am wrong
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  
  # get() returns the matrix x
  get <- function() {
    x
  }
  
  # When setsolve(s) is called in cacheSolve,
  # s contains the calculated matrix inverse.
  # s is a local variable defined in cacheSolve at this moment.
  # After executing setsolve(s), <<- assigns s to a variable
  # with the same name s. <<- has made s a global variable now.
  setsolve <- function(solveResult) {
    s <<- solveResult
  }
  
  # getsolve() returns the value of s
  # Value of s is originally NULL because a function's free variables
  # get their values from the function's parent environment.
  # If setsolve() has been called, getsolve's s will get the value
  # of setsolve's global variable s which is the inverse of the matrix
  getsolve <- function() {
    s
  }
  
  # After defining the functions above, return a list of them.
  # Names of the list elements are the same as the functions
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


# `cacheSolve`: This function computes the inverse of the special
# "matrix" returned by `makeCacheMatrix` above. If the inverse has
# already been calculated (and the matrix has not changed), then
# `cacheSolve` should retrieve the inverse from the cache.

# z contains the list returned by makeCacheMatrix(x) 
cacheSolve <- function(z, ...) {
  
  # s is a variable representing the inverse of matrix x 
  
  # Try to see what z$getsolve() resturns to s
  s <- z$getsolve()
  
  # If the inverse has already been calculated, 
  # s will get the inverse.
  # Otherwise, s will get NULL
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
    # return() also exits cacheSolve().
    # and stops the followings from being executed.
  }
  
  # The followings will be executed only if a cache does not exist
  # and s is still NULL before executing the followings 
  
  # z$get() returns the matrix x to data
  data <- z$get()
  
  # solve(data) returns the inverse of data i.e. matrix x to s
  # s is a local variable at this moment.
  s <- solve(data, ...)
  
  # Store the inverse in the cache
  z$setsolve(s)
  
  # Return the inverse
  # z$setsolve(s) has made s a global variable now
  s
}