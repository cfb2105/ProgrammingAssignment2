## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates an output of lists of functions that can be used to set and get the matrix as well as set and get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
 i <- NULL ##sets the variable for the inverse equal to null and makes it available for future functions
  set <- function(y,num_columns, num_rows) { 
    x <<- matrix(y, nrow = num_columns, ncol = num_rows) ##sets the matrix x equal to y with the number of columns and rows specified
    i <<- NULL ## resets the inverse when a new matrix is set
  }
  get <- function() x ##allows the user to get the value of the matrix set in cache
  setinverse <- function(inverse) i <<- inverse ## sets the value of the inverse equal to the inputted number
  getinverse <- function() i ##retrieves the value of the inverse
  ##output the list of functions with respect to the matrix
output <<-list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Checks the cache to see if the inverse has been cached and returns the cached value, if the cache is empty it calculates the inverse and puts it in cache

cacheSolve <- function(x, ...) {
         i <- x$getinverse() ##retreieves the inverse matrix from cache
  if(!is.null(i)) { 
    message("getting cached data")
    return(i)
  } ##if inverse matrix has been cached returns a message that the inverse is being retrieved from cache along with the cached value
  data <- x$get() ##retrieves the cached matrix
  i <- ginv(data, ...) ##takes the inverse of the matrix
  x$setinverse(i) ##caches the inverse of the matrix
  i
}
