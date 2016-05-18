## Cacheing the inverse of a matrix
## This function creates the special matrix
makeCacheMatrix <- function(x = numeric()) {
  inv <- NULL
#`<<- assigns a value to an object in the environment
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,setinv = setinv, getinv = getinv)
}
## This function takes the inverse of the special matrix

cacheSolve <- function(x, ...) {
# this returns the inverse of the original matrix
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat.data <- x$get()
  inv <- solve(mat.data, ...)
# This sets the value of the inverse in the cache via the setinv function
  x$setinv(inv)
  return(inv)
}

## Test this
my_matrix <- makeCacheMatrix(matrix(1:10, 2, 2))
my_matrix$get()
my_matrix$getinv()
cacheSolve(my_matrix)
my_matrix$getinv()
