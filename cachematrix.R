## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# makeCacheMatrix
# The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to
# 1. set the value of the matrix (Using the same pattern as example)
makeCacheMatrix <- function(x = matrix()) {
# Set inverse of x to be NULL
    invx <- NULL
# Set the function of y 
    set <- function(y) {
        x <<- y
        invx <<- NULL
    }
# 2. get the value of the matrix
    get <- function() x
# 3. set the value of the inverse
    setinv <- function(inv) invx <<- inv
# 4. get the value of the inverse
    getinv <- function() invx
# List them all 
    list(set = set, get = get, 
         setinv = setinv, 
         getinv = getinv)
}


## Write a short comment describing this function

# cacheSolve
# Using the same pattern as example
cacheSolve <- function(x, ...) {
# Get inverse of x
  invx <- x$getinv()
# Check if there is NULL in inverse of x or not
  if(!is.null(invx)) {
    message("getting cached data.")
# Return inverse of x
    return(invx)
  }
# Set Data by getting matrix x
  data <- x$get()
  # Using solve command instead of mean command in example to solve inverse square matrix
  invx <- solve(data)
  x$setinv(invx)
# Show the result of inverse of matrix x
  invx
}
