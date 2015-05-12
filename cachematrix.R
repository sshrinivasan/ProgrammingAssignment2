## Computing the inverse of a matrix
##
## These functions compute the inverse of a square, non-singular matrix
## The inverse is cached, and if the matrix has not been changed, the cached value
## is returned without needing to re-compute the inverse.

## Creates an instance of a special type of matrix, which contains
## 1. The matrix itself
## 2. A set of four functions that set and get the matrix and its inverse
## 
## Usage : newMatrix = makeCacheMatrix(matrix(rnorm(4), 2, 2))
## 
## > newMatrix$set(matrix(rnorm(4), 2, 2))    -> set the value of the matrix
## > newMatrix$get()                          -> get the matrix
## > newMatrix$setinv(matrix(rnorm(4), 2, 2)) -> set the inverse
## > newMatrix$getinv()                       -> get the inverse

makeCacheMatrix <- function(x = matrix()) {
  # Set the inverse the be NULL initially
  inv <- NULL
  # Function to set the value of the matrix and reset the inverse to NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  # Return the matrix
  get <- function() x
  # Set the inverse
  setinv <- function(inverse) inv <<- inverse
  # Return the inverse
  getinv <- function() inv
  
  # Return a list of four functions to manipulate this new type of matrix
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Function to compute the matrix inverse.
##
## If the inverse has been computed and the matrix has not changed, return the cached value
## If the matrix has changed, compute the inverse

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  
  # If the inverse is not NULL, it means the iverse has been cached. So return it
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # If the inverse is NULL, it means that the data has changed. Compute the inverse
  # Get the raw matrix itself
  data <- x$get()
  # Solve for the inverse
  inv <- solve(data, ...)
  # Update the cache with the new value of the inverse
  x$setinv(inv)
  # Return the computed inverse
  return(inv)
}
