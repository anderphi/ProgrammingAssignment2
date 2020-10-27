## These two functions optimise the calculation of the inverse of a matrix by 
## cahing it the first time that the inverse is calculated.


## makeCacheMatrix returns a list of the functions: set/get matrix and get/set inverse. 

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve has as inputs the list of functions created at makeCacheMatrix and
## returns the matrix inverse (by taking from the cache or calculating it)

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  matrix <- x$get()
  i <- solve(matrix,...)
  x$setinverse(i)
  i
}