makeCacheMatrix <- function( m = matrix() ) {
  
  ## Initialize the inverse property
  i <- NULL #Set as null so can be used within function later
  
  # Take the matrix passed into makeCacheMatrix and assign a value to an object in an environment
  # that is different from the current environment through the <<- operator
  # This object will be referenced in the cacheSolve function below through the functions defined further below
  
  ## Method to set the matrix
  set <- function( matrix ) {
    m <<- matrix
    i <<- NULL
  }
  # The three functions below enables us to get the value of the matrix passed as an argument above
  # or provide the code to set and get the inverse values
  
  ## Method the get the matrix
  get <- function() {
    ## Return the matrix
    m
  }
  
  ## Method to set the inverse of the matrix
  setInverse <- function(inverse) {
    i <<- inverse
  }
  
  ## Method to get the inverse of the matrix
  getInverse <- function() {
    ## Return the inverse property
    i
  }
  #Declaration of functions to make them availabe as objects in cacheSolve()
  #The first element is the method name that is used to access the function
  
  ## Return a list of the methods
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Compute the inverse of the special matrix returned by "makeCacheMatrix"
## above. If the inverse has already been calculated (and the matrix has not
## changed), then the "cachesolve" should retrieve the inverse from the cache.
# The following function is where the computations are performed to check whether
# the matrix was inversed. If so, get it from cache, otherwise invert it.
cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  
  # Check if matrix was previously inversed and return if not null
  if( !is.null(m) ) {
    message("getting cached data")
    return(m)
  }
  
  ## Get the matrix from our object
  data <- x$get()
  
  ## Calculate the inverse using matrix multiplication
  m <- solve(data) %*% data
  
  ## Set the inverse to the object
  x$setInverse(m)
  
  
  # Return the result to the console
  m
}