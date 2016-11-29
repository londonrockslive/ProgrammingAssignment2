## Put comments here that give an overall description of what your
## functions do

## function which allows you to create a cached inverse of matrix and get it

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  # set value of inverse
  setInverse <- function() m <<- solve(m)
  # get value of inverse
  getInverse <- function() m
  # output: list of functions to set matrix, get matrix, set inverse, and get inverse
  list(set=set, get=get,
       setInverse = setInverse,
       getInverse = getInverse)

}


## function which allows you to get inverse of matrix and if already cached to retrieve from cache
    
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()
     ## check if 'x' exists in cache/if m is now inverse
    if(!is.null(m)) {
         ## you already have the inverse, message, then return matrix
      message("getting cached data")
      return(m)
    }
    ## inverse not cached, get original matrix then calculate inverse
    data <- x$get()
    m <- solve(data, ...)
    m
}
