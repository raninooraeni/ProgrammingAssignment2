## Below are two functions that are used to create a special object 
## that stores a matrix and caches the inverse of the matrix

## The first function creates a special "matrix" object 
## that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) {
    if (det(x)==0) {
      stop("det matrix = 0 can not be inverted")
    }
    if (dim(x)[1]!=dim(x)[2]) {
      stop("matrix must be square")
    }
    s <<- solve
  }
    
  getsolve <- function() s
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## The second function computes the inverse of the special "matrix"

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  s <- x$getsolve()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  s
}
