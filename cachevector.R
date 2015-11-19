makeVector <- function(x = numeric()) {
  
  ## initialize a variable to contain the mean of the vector
  m <- NULL
  
  ## store the data vector to the special vector and initialize the mean variable
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  ## retrieve the data vector from the special vector
  get <- function() x
  
  ## assign the mean calculated to the special vector
  setmean <- function(mean) m <<- mean
  
  ## retrieve the mean from the special vector
  getmean <- function() m
  
  ## the function returns a list with the following four items:
  ##  the function for setting the data
  ##  the function for getting the data
  ##  the funciton for setting the calculated mean
  ##  the function for getting the calculated mean
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}


cachemean <- function(x, ...) {
  m <- x$getmean()
  
  if(!is.null(m)) {
    message('getting cached value')
    return(m)
  }
  
  data <- x$get()
  m <- mean(data, ...)
  x$setmean(m)
}