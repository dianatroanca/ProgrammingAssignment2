## these functions will cache the inverse of a matrix so it doesn't get computed every time

## creates a special matrix which is a list containing a fct to
##	set the value of the matrix
## 	get the value of the matrix
## 	set the value of the inverse
##	get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  
  get <- function() x
  
  setInverse <- function(inverse) i <<- inverse
  
  getInverse <- function() i
  
  list( set = set, get = get, setInverse = setInverse, getInverse = getInverse)

}


##the following function calculates the inverse of a matrix of the previously defined type; if the inverse is cached it returns the cached value, otherwise it computes the inverse and caches the value
##first I considered also cases when there is no inverse, but those lines are commented here as the instructions said to ignore those cases

cacheSolve <- function(x, ...) {
  #getting the cached inverse
  inverse <- x$getInverse()
  
  #if it is not null, return cached value
  if(!is.null(inverse)){
   message("getting cached inverse")
   return(inverse)
  }
  
  #there is no cached inverse, get value for matrix
  data <- x$get()
  
  #function to check if matrix has an inverse
  #f <- function(m) {
  #  class(try(solve(m),silent=T))=="matrix"
  #}
  
  #compute inverse if it exists
  #if(f(data)){
    inverse <- solve(data)
    x$setInverse(inverse, ...)
    inverse
  #}
  #else{
  #  "there is no inverse"
  #}

}
