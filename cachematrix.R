## Put comments here that give an overall description of what your
## functions do

## Creating a function that creates a matrix object that can casche its inverse.


makeCacheMatrix <- function(x = matrix()){
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solved) m <<- solved
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}

#This function calculates the inverse of a the special matrix object created with makeCacheMatrix
#It first checks to see if the inverse has already been calculated, if so it gets the inverse from the cache with the 
# getInverse function and skips the calculation. Then it prints out the message "getting cached data".
#Otherwise it calculates the inverse of the matrix and sets the value of the inverse with the setInverse function.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

