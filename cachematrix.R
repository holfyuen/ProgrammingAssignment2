## These two function saves time in repeatedly calculating inverse of matrix
## by caching ones already calculated 
## Step 1: abc <- makeCacheMatrix(your_m), where your_m is the matrix you have
## Step 2: cacheSolve(abc)

## makeCacheMatrix returns a list of four objects
## set: assign a value to a variable
## get: get an existing variable
## setinv: run the inverse of matrix
## getinv: get the inverse of the matrix already calculated

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


## If m is not null, get cached data
## If it is, call the solve function to calculate inverse

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
  ## Return a matrix that is the inverse of 'x'
}
