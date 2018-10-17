## We first make a function to set and get the values of the matrix
## Nextk, set and get the inverse matrix to be used as input for cacheSolve() below 
## x is a square, invertible matrix 

makeCacheMatrix <- function(x = matrix()) {
  
  inv = NULL
  set = function(y) {
    x <<- y
    inv <<- NULL
  }
  get = function() x
  setinv = function(inverse) inv <<- inverse 
  getinv = function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}

##This function computes the inverse of the matrix returned by makeCacheMatrix above. 
##If the inverse has already been calculated - then it should retrieve this from the cache and return the message "getting cached data"

cacheSolve <- function(x, ...) {
  inv = x$getinv()
  if (!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  
  data = x$get()
  inv = solve(data, ...)
  
  x$setinv(inv)
  
  return(inv)
}
