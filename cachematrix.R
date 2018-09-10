## makeCacheMatrix() and cacheSolve() can be used to cache the inverse of a 
## matrix in memory in order to avoid having to repeatedly perform the inverse
##  computation


## makeCacheMatrix() creates a special "matrix" object that can cache its 
## inverse


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse<- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## cacheSolve() is required to populate and/or retrieve the inverse from an object 
## of type makeCacheMatrix()

cacheSolve <- function(x, ...) {
        ## Returns a matrix that is the inverse of 'x', if the inverse has
        ## already been solved it will retrieve it from chached data
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
