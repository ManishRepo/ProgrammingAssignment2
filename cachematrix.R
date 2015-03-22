## makeCacheMatrix function creates a wrapper over the matrix that makes it cachable its inverse
## The function takes the matrix and adds a inverse dimension/variable to new matrix definition
## get and set are provided to query these values(matrix and its inverse)

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


## cachesolve function checks for the cached inverse of the matix in the cacheable matrix wrapper definition
## if found that value is returned and if not the inverse is computed and
## cached for the future access.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data)
  x$setinverse(i)
  i
}
