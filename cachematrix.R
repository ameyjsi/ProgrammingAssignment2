## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL					## Initializing inverse
  set <- function(m2) {				## defining the set function to assign matrix
    x <<- m2    ## Assigning value of matrix from other environment
    inverse <<- NULL
  }
  get <- function() x 	##defining get function
  setInverse <- function(inveresed_value) inverse <<- inveresed_value ##defining SetInverse function
  getInverse <- function() inverse  ##defining getInverse function
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function

##This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then cacheSolve will retrieve the inverse from the cache
cacheSolve <- function(x, ...) {
  inv <- x$getInverse() ## Assiging value of inversed matrix
  if(!is.null(inv)) {   ## If it is not null then returns inversed matrix
    message("getting cached data")
    return(inv)
  }
  ## If it is null then cursor will come here and compute the reverse of matrix
  matrix_data <- x$get() ## Gets the matrix value
  inv <- solve(matrix_data, ...) ## Reverses the value
  x$setInverse(inv) ## Set the reversed value
  inv
}

