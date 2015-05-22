## 'makeCacheMatrix' creates a "special" matrix
## which is really a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL ## initialize the inverse matrix value
  
  set <- function(y) { # set the value of the matrix
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x # get the value of the matrix
  
  set_inv <- function(inv_input) inv <<- inv_input # set the value of the inverse
  
  get_inv <- function() inv # get the value of the inverse
  
  list(set = set, get = get, 
       set_inv = set_inv, get_inv = get_inv) # return a list of the above functions
}


## 'cacheSolve' calculated the inverse of the 'matrix' created 
## with 'makeCacheMatrix'
## It first varifies if the inverse was already created
## If so, it gets the inverse and skips the computation
## If not, it calculated the inverse of the matrix and sets the 
## value of the inverse in the cache via the 'set_inv' function

cacheSolve <- function(x, ...) {
  
  inv <- x$get_inv() # checks for inverse and gets it if it exists
  if(!is.null(inv)) {
    message("getting cached inverse")
    return(inv)
  }
  data <- x$get() # gets the matrix if above is FALSE
  
  inv <- solve(data, ...) # calculates the inverse of the matrix
  
  x$set_inv(inv) # cache the inverse
  
  inv # return the results of the inverse for the matrix
}
