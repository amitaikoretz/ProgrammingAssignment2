## Put comments here that give an overall description of what your
## functions do

## This function creates a representation for a matrix with cached inverse

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


## This function gives the inverse of the matrix represented by x, using the cached inverse

cacheSolve <- function(x) {
  
#   A<-matrix(runif(16),nrow=4,ncol=4)
  
  inv<-x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinv(inv)
  inv
  
  }
