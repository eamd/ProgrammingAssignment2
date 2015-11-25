##
## makeCacheMatrix
## parameters: x - a matrix, the defaut value is a matrix with no elements
## 
## Caches the inverse of the matrix passed to it. By caching the inverse,
## as long as the matrix doesn't change, the function won't have to recaculate
## the inverse. If a different matrix is passed, the inverse will need to be
## recacluated.
##
## If no matrix is passed, the function will default to an empty matrix.
## If something other than a matrix is passed, such as a numeric vector,
## the function will return NULL. That way, calling functions can
## handle the error gracefully.
##
makeCacheMatrix <- function(x = matrix()) {

  ##
  ## Check to make sure that what is passed in the x variable is a matrix
  ## If x is not of class, matrix, a message is returned to the user indicating the status
  ## and the function will return the value of x.
  ##
  if(!is.matrix(x)) {
    message('x must be a matrix')
    x <- NULL
    return(x)
  }
  

  ##
  ## im is the the variable for holding the cached inverse matrix.
  ## im is initialized to NULL.
  ##
  im <- NULL
  
  ##
  ## There are 4 helper functions for setting and retrieving from the special matrix object"
  ##
  ## set(y) - assign the matrix to the special matrix object
  ## setinverse(im) - assign the inverse of the passed matrix, to the im cache variable
  ##
  ## get() - retrieve the matrix stored in the special matrix object
  ## getinvers() - retrieve the inverse matrix from the im cache variable
  ##
  
  set <- function(y) {
    x <<- y # assign the value of y to x located in the parent environmentr
    im <<- NULL # initialize im in the parent environment (cache of the inverse) to NULL
  }
  
  get <- function() {
    x # retrieve the matrix stored in the special matrix object and return to the function name get
  }

  setinverse <- function(matrix) {
    im <<- matrix # calculate the inverse of the matrix and place in the im cache variable
  }
  
  getinverse <- function() {
    im # return the value of the inverse cache, im to the function name getinverse
  }
  
  
  #
  # return a list of four elements
  #
  list(get=get, set=set, setinverse=setinverse, getinverse=getinverse)
    
}


## Write a short comment describing this function
##
## cacheSolve
## parameters: at least one matrix, x
## 
## cacheSolve computes the inverse of a matrix passed to it. The matrix is stored as a special
## matrix created by the function makeCacheMatrix
##
cacheSolve <- function(x, ...) {
  # Check to make sure the inverted matrix hasn't already been computed
  tmpM <- x$getinverse()
  
  if(!is.null(tmpM)) {
    message('retrieving cached inverted matrix') # the inverse has already been computed, return it
    return(tmpM)
  }

  # the inverse needs to be computed.
  tmpM = x$get() # get the original matrix
  im <- solve(tmpM) # compute the inverse
  x$setinverse(im)
  im
}

