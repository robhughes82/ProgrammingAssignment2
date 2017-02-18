## The functions produce the inverse of a matrix, if the matrix has been previously calculated then
## it is taken from the Cache, otherwise it is calculated by the function

## makeCachematrix creates 4 functions to be used within cacheSolve, to set the matrix and the inverse to the 
## Cache, and to retrieve the matrix and the inverse from the Cache

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(sol) inv <<- sol
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}


## cacheSolve firstly checks if the inverse matrix is in the cache, if it is then it returns it from there
## if not it calculates the inverse of the matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
