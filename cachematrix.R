## Put comments here that give an overall description of what your
## functions do

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
####   Write a short comment describing this function    ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

makeCacheMatrix <- function( m = matrix() ) {
  
  ## Initialize the inverse property
  i <- NULL
  
  ## Method to set the matrix
  set <- function( matrix ) {
    m <<- matrix
    i <<- NULL
  }
  
  ## Method the get the matrix
  get <- function() {
    m
  }
  
  ## Method to set the inverse of the matrix
  setInverse <- function(inverse) {
    i <<- inverse
  }
  
  ## Method to get the inverse of the matrix
  getInverse <- function() {
    i
  }
  
  ## Return a list of the methods
  out <- list(set = set, get = get,
              setInverse = setInverse,
              getInverse = getInverse)
  
  return(out)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  
  y <- makeCacheMatrix(x)
  
  ## Return a matrix that is the inverse of 'x'
  m <- y$getInverse()
  
  ## Just return the inverse if its already set
  if( !is.null(m) ) {
    message("getting cached data")
    return(m)
  }
  
  ## Get the matrix from our object
  data <- y$get()
  
  ## Calculate the inverse matrix
  m <- solve(data)
  
  ## Set the inverse to the object
  y$setInverse(m)
  
  ## Return the matrix
  return(m)
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
####   Applying the functions   ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# A dummy 3 x 3 matrix
dummy_mat <- matrix( c(2,5,4,9), 2, 2 )

# Apply makeCacheMatrix into dummy_mat
cacheSolve(dummy_mat)
