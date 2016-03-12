## Put comments here that give an overall description of what your
## functions do


## Make a "matrix" which is actually a list containing a function to
## set and get the values of the matrix and the inverse

makeCacheMatrix <- function(x = matrix()) {
  m<- NULL
  set <- function(y) {
    x<<- y
    m<<- NULL
  }
  get <-function() x
  setinverse <- function(solve) m<<-solve
  getinverse <- function() m
  list(set = set, get=get,
       setinverse=setinverse,
       getinverse=getinverse)
}


## Compute the inverse of the matrix unless it has already been computed,
## in which case return cached value

cacheSolve <- function(x, ...) {
    m<-x$getinverse()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m<-solve(data,...)
    x$setinverse(m)
    m
        ## Return a matrix that is the inverse of 'x'
}

