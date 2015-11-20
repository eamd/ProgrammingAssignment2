## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix prepares a list construct to represent a matrix object that has the 
## the ability to cache an already calculated inverted matrix.
##
## The function has four functions total - two for getting and setting the original matrix
## and two for getting and setting the calculated inverted matrix of the original matrix.
##
## get: retrieves the stored matrix from within the function's environment.
## set: stores the passed matrix within the special matrix function's environment.
##
## getinverse: restrieves the inverted matrix stored within the function's environment.
## setinverse: stores the inverted matrix within the function's environment.
##
## The function returns an R list, with each element in the list a function definition that is executed when
## called. To reference these functions, list_variable_name$function()
##
makeCacheMatrix <- function(x = matrix()) {

  
    if(!is.matrix(x)) {
      message('x must be a matrix')
      return(x)
    }
    basem <- x
  
    # The cached matrix variable. Calling functions can check if this is set, and
    # if it is, avoid calling the solve function repeatedly.
    # This variable is reset to null every time a new matrix is created.
    im <- NULL

    
    set <- function(y) {
      x <<- y # assign matrix define to a local variable, x
      print(y)
      im <<- NULL
    }
    
    get <- function() x
    
    #
    # apply the solve function passed in the 'im' parameter
    # the result of solve is assign to the 'im' variable 
    # defined as part of makeCacheMatrix
    #
    setinverse <- function(im) {
      im <<- solve(im)
    }
    
    #
    # Returns the value store in 'im' defined at the function level. The result is returned
    # as a matrix.
    #
    getinverse <- function() im
    
    #
    # Return the a list that define the four functions needed to get/set 
    # the original matrix and the inverted matrix
    # This forces the user of the function to access or set data in a controlled manner
    #
    list(get=get, set=set, setinverse=setinverse, getinverse=getinverse)
    
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  # Check to make sure the inverted matrix hasn't already been calculated
  tmpM <- x$getinverse()
  if(!is.null(tmpM)) {
    message('retrieving cached inverted matrix')
    return(im)
  }

  tmpM = x$get()
  im <<- x$setinverse(tmpM)
  return(im)
  }
