
# makeCacheMatrix: return a list of functions to:
# 1. Set the value of the matrix
# 2. Get the value of the matrix
# 3. Set the value of the invMerse
# 4. Get the value of the invMerse
makeCacheMatrix <- function(x = matrix()) {
  # invM will store the cached invMerse matrix
  invM <- NULL
  
  # Setter for the matrix
  set <- function(y) {
    x <<- y
    invM <<- NULL
  }
  # Getter for the matrix
  get <- function() x
  
  # Setter for the invMerse
  setinvM <- function(invMerse) invM <<- invMerse
  # Getter for the invMerse
  getinvM <- function() invM
  
  # Return the matrix with our newly defined functions
  list(set = set, get = get, setinvM = setinvM, getinvM = getinvM)
}


# cacheSolve: Compute the invMerse of the matrix. If the invMerse is already
# calculated before, it returns the cached invMerse.
cacheSolve <- function(x, ...) {
  invM <- x$getinvM()
  
  # If the invMerse is already calculated, return it
  if (!is.null(invM)) {
    message("getting cached data")
    return(invM)
  }
  
  # The invMerse is not yet calculated, so we calculate it
  data <- x$get()
  invM <- solve(data, ...)
  
  # Cache the invMerse
  x$setinvM(invM)
  
  # Return it
  invM
}