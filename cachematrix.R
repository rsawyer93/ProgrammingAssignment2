## makeCacheMatrix sets up a special matrix that can be used in the casheSolve function
## If the matrix passed to the casheSolve function is new, then the function will calsulate, 
## store and return the inverse of the matrix. If it is not new, it will find the inverse of
## the matrix in the cache and return its value


## sets up a special matrix that can be used in the casheSolve function

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)

}


## takes the makeCacheMatrix object as input and searches for inverse of matrix in cache if previously
## been calculated, if not it will calculate and store the inverse of the matrix in the cache.

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
    }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
