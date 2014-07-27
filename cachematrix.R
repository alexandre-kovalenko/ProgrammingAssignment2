## Functions below create special matrix object which caches 
## matrix' inverse thus allowing consumer to speed up potentially
## lengthy operation if it is repeated more than once.
##
## NOTE: this implementation will only deal with the square matrices

## Create partially fixed matrix object

makeCacheMatrix <- function(x = matrix()) {
  if(dim(x)[1] != dim(x)[2])
    stop("Will only invert square matrices, sorry...")
  cachedInverse <- NULL
  set <- function(m) {
    x <<- m
    cachedInverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) cachedInverse <<- inverse
  getinverse <- function() cachedInverse
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Use partially fixed matrix object to compute the inverse

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  result <- x$getinverse()
  if(!is.null(result))
  {
    return(result)
  }
  data <- x$get()
  result <- solve(data)
  x$setinverse(result)
  result
}
