## week3 exercise 
## create a matrix object that caches the inverse of the matrix

## function get a mtrix aand return a list object with get, set,
## getnverse and setInverse. the object is used by the cacheSolve function
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## function gets a object that is returned by the makeCacheMatrix function
## the function returns the inverse of the matrix, first checks if the result is cached,
## if so it returns the cached result, if not calcultes the inverse and caches the result 

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
