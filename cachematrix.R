## Matrix inversion is usually a costly computation. The following pair of functions caches the inverse of the matrix
## to make the computation more efficient.


## The function makeCacheMatrix creates a matrix containing  a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the inverse of the matrix
## 4. get the inverse of the matrix



makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function() m <<- solve(x)
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The function CacheSolve computes the inverse of the matrix returned by makeCacheMatrix. 
## If the inverse has already been calculated(and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the Cache. 

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  
  ## it returns the inverse of 'x'
  m
}






