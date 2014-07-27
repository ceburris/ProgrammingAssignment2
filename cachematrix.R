## Create and cache the inverse of a matrix.
## If inverse has already been solved, uses cached value.

## Create matrix, store inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
      x <<- y
      m <<- NULL
  }
  ##Function to get matrix
  get <- function() x
  ##Sets cache of inverse
  setinv <- function(z) {
    m <<- z
  }
  ##Returns value of cached inverse
  getinv <- function() m
  list(set=set,get=get,setinv=setinv,getinv=getinv)
}


## Check if inverse exist, if not calculate

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  ##Check if cached inverse exists, if so return
  m <- x$getinv()
  if(!is.null(m)){
      message("getting cached data")
      return(m)
  }
  ##Calculate inverse and store cache
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}

