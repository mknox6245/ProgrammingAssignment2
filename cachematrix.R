# These functions are used to cache the inverse of a matrix to speed computation time by
# not requiring that the inverse be recalculated each time the fuction is called.

# Create the matrix to which the inverse will be cached
makeCacheMatrix <- function(x = matrix()) {
  inv = NULL
  set = function(y) {
    x <<- y
    inv <<- NULL
  }
  get = function() x
  setinv = function(inverse) inv <<- inverse 
  getinv = function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}

}

# Solve for the inverse of the matrix
cacheSolve <- function(x, ...) {
  inv = x$getinv()
  
  # Check if inverse already calculated
  if (!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  
  # Calculate the inverse of the matrix & cache
  mat.data = x$get()
  inv = solve(mat.data, ...)
  x$setinv(inv)
  
  return(inv)
}
