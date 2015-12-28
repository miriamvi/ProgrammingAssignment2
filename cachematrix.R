## Solving the Inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Getting the Cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    print("getting cached data")
    return(m)
  }
  print("solving inverse matrix")
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
x<-matrix(1:4,2,2)
y<-makeCacheMatrix(x)
cacheSolve(y)
cacheSolve(y)
