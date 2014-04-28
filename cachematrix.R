## makeCacheMatrix: This function creates a special matrix object that can cache its inverse.
## cacheSolve: This function computes the inverse of the special matrix returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cacheSolve should retrieve the inverse from the cache.

## makeCacheMatrix creates a list of functions to
## 1. set the matrix
## 2. get the matrix
## 3. set the inverse matrix
## 4. get the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  ## set the matrix
  set <- function(y){
    x <<- y
    inverse <<- NULL
  }
  ## get the matrix
  get <- function() x
  ## set the inverse matrix
  setInverse <- function(inverse) inverse <<- inverse
  ## get the inverse matrix
  getInverse <- function() inverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## cacheSolve returns a matrix that is the inverse of 'x'
## 1. It first checks to see if the inverse matrix has already been calculated
## 2. If so, it gets the inverse matrix and skips the computation
## 3. Otherwise, it calculates the inverse matrix and saves it in the cache

cacheSolve <- function(x, ...) {
  ## first check to see if the inverse matrix has already been calculated
  inverse <- x$getInverse()
  if(!is.null(inverse)) {
    ## get the inverse matrix from the cache
    message("getting cached data")
    return(inverse)
  }
  ## calculate the inverse matrix and save it in the cache
  data <- x$get()
  inverse <- solve(data)
  x$setInverse(inverse)
  inverse
}
