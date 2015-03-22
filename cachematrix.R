## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) { ##creates a function called makeCacheMatrix that takes a matrix as the only argument
  
  s <- NULL                                 ##sets variable s to NULL in the global environment
  set <- function(y) {                      ##creates a function named 'set' which takes free variable 'y' and sets 'x' to 'y' in the global environment
    x <<- y                                 
    s <<- NULL                              ##sets 's' to NULL in the global environment
  }
  get <- function() x                       ##creates a function named 'get' that returns 'x' (the original matrix)
  setinv <- function(solve) s <<- solve     ##creates a function named setinv which calculates the inverse of x and changes the value of 's' in the global 
  ##environment to be the inverse of the matrtix 'x'
  getinv <- function() s                    ##creates a function named 'getinv' that returns 's'
  list(set = set, get = get,              ##creates a list with 4 named values which return the functions that they share their names with 
       setinv = setinv,
       getinv = getinv)
}

cacheSolve <- function(x, ...) {
  
  s <- x$getinv()                           ##sets 's' in the current environment to the result of the getinv function from above
  if(!is.null(s)) {                         ##checks to see if s is NULL and returns the matrix s if it is not
    message("getting cached data")          
    return(s)
  }
  data <- x$get()                           ##if 's' returns NULL, retrieves the original matrix, solves it, updates the list and then returns 's' (the inverted matrix)
  s <- solve(data, ...)
  x$setinv(s)
  s
  
}
