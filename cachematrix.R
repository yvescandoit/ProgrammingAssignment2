#new comment 
## The two functions here MakeCacheMatrix & CacheSolve help in computing the inverse of a matrix 
## These function helps in storing data in cache in order to save computng time
## This function called MakeCacheMatrix creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
inv <- NULL
  set <- function(y) { 
    x <<- y
  inv <- NULL 
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inv
  getinverse <- function() inv
  list(set = set,get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function CacheSolve helps in computing the inverse of the special matrix created by above function
#the function first checks whether the matrix has already been computed and stored in the cache
#if yes then it gets the data from cache else it computes it

cacheSolve <- function(x, ...) {
 inv <- x$getinverse()
  if (!is.null(inv)) {
    print("getting cache data")
    return(inv)
  }
  data <- x$set()
  inv <- solve(data,...)
  x$setinv(inv)
  inv       
}
