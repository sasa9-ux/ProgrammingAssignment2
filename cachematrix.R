## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## EXPLANATORY COMMENT:
## makeCacheMatrix is assigned the function "matrix"
## This can therefore be used to cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function

## EXPLANATORY COMMENT:
## cacheSolve is used to return and compute the inverse of the special "matrix"
## This will be based on the code of makeCacheMatrix 
## However, if the inverse has been calculated before, this will retrieve the inverse value from the cache

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("Getting Cached Inverse Data")
    return(inv)
  }
  
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  
  inv
}

## Return a matrix that is the inverse of 'x'

### TEST
test <- matrix(c(2,5,11,6,4,14,15,20), 3,3)

test1 <- makeCacheMatrix(test) 

cacheSolve(test1)
