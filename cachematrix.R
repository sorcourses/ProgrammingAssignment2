#
# Cache the inverse of matrix
#
# The functions makeCacheMatrix() and cacheSolve() allow respectively
# to construct inverse-cacheable matrices and to get the inverse
# repeateadly without calculating it over.
#
# Example of usage
#
# > my_matrix = makeCacheMatrix(matrix(c(1,2,3,4), ncol=2, nrow=2))
# > my_matrix$get()
#      [,1] [,2]
# [1,]    1    3
# [2,]    2    4
# > cacheSolve(my_matrix)
#      [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
#

#
# Create matrices with inverse caching ability
#
makeCacheMatrix <- function(x = matrix()) {      

  i <- NULL                  # the inverse has not been calculated

  #
  # Link a new matrix y as a new value for x in this closure
  # This particular method could be safely removed for most usages
  #              
  set <- function(y) {
    x <<- y                  # assign the new matrix to the parameter
    i <<- NULL               # causes the inverse to be outdated
  }
  
  #
  # Get the value of the matrix
  #
  get <- function() {
    x                        # returns the value of x
  }

  #
  # Set the value for the inverse matrix, once calculated
  #
  setinv <- function(inv) {
    i <<- inv                # assign the new inverse
  }
  
  #
  # Get the inverse of the matrix
  #
  getinv <- function() {
    i                        # get the inverse. Initially returns NULL
  }

  list(set    = set, 
       get    = get,
       setinv = setinv,
       getinv = getinv)      # returns the list with the inner functions
}

#
# Invert cacheable matrices, returning the inverse while storing its value
#
cacheSolve <- function(x, ...) {

  if(!is.null(x$getinv())) { # if exists already an inverse for x$get() 
    ## message("Cache hit")  # test (disabled for production)
    return(x$getinv())       # return the inverse cached
  }
  
  ## message("Cache miss")   # test (disabled for production)
  inv <- solve(x$get(), ...) # calculate the inverse matrix
  x$setinv(inv)              # set the inverse matrix for x$get()
  inv                        # return the inverse matrix
}
