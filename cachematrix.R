## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# makeCacheMatrix
# The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to
# 1. set the value of the matrix (Using the same pattern as example)
makeCacheMatrix <- function(x = matrix()) {
    invx <- NULL
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
    list(set = set, get = get, 
         setinv = setinv, 
         getinv = getinv)
}


## Write a short comment describing this function

# cacheSolve
# Using the same pattern as example
cacheSolve <- function(x, ...) {
  invx <- x$getinv()
  if(!is.null(invx)) {
    message("getting cached data.")
    return(invx)
  }
  data <- x$get()
  # Using solve command instead of mean command in example to solve inverse square matrix
  invx <- solve(data)
  x$setinv(invx)
  invx
}
