## makeCacheMatrix and cashSovle intended to be a pair of functions
## that cache the inverse of a matrix

## Make a CacheMatrix and
## return a list containing functions to
##              "Set the matrix"
##              "Get the matrix"
##              "Set the inverse"
##              "Get the inverse"
## which can later be used as the inputs of cacheSolve()

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) 
    inv <<- inverse 
  getinv <- function() inv
  list(set=set, 
       get=get, 
       setinv=setinv, 
       getinv=getinv)
}


## casheSolve intended to inverse of the original matrix input to makeCacheMatrix()

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  
  # if the inverse has already been calculated: get it from the cache and skips
  # the computation.
  if (!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  
  # if the invers has not been calculated: calculates the inverse 
  data <- x$get()
  inv <- solve(data, ...)
  
  # sets the value of the inverse in the cache via the setinv function.
  x$setinv(inv)
  inv
}
