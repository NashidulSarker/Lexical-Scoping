# Function to create a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # Initialize the cache for the inverse
  
  # Function to set the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL  # Reset inverse cache when the matrix changes
  }
  
  # Function to get the matrix
  get <- function() {
    x
  }
  
  # Function to set the inverse of the matrix
  setInverse <- function(inverse) {
    inv <<- inverse
  }
  
  # Function to get the inverse of the matrix
  getInverse <- function() {
    inv
  }
  
  # Return a list of the functions
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


# Function to compute the inverse of the matrix, with caching
cacheSolve <- function(x, ...) {
  inv <- x$getInverse()  # Check if the inverse is already cached
  
  # If the inverse is cached, return it
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # Otherwise, calculate the inverse
  mat <- x$get()  # Get the matrix
  inv <- solve(mat, ...)  # Compute the inverse
  x$setInverse(inv)  # Cache the inverse
  
  return(inv)  # Return the inverse
}
