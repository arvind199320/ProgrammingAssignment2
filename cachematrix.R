## makeCachematrix creates a matrix object whose inverse can be cached
##cachesolve creates an inverse of the matrix object retured byy makeCachematrix.
##Provided the inverse has already been calculated and the parent matrix object 
##is intact, the inverse should be retrieved from cache

## makeCachematrix creates a matrix object that can be cached

makeCacheMatrix <- function(x = matrix()) {
  a <- NULL
  set <- function(y){
    x <<- y
    a <<- NULL
  }
  get <- function()x
  setInverse <- function(inverse) a <<- inverse
  getInverse <- function() a 
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}

cacheSolve <- function(x, ...) {
##cachesolve computes inverse of a matrix, using the solve function.  
  a <- x$getInverse()
  if(!is.null(j)){
    message("getting cached data")
    return(a)
  }
  mat <- x$get()
  a <- solve(mat,...)
  x$setInverse(a)
  a
}


