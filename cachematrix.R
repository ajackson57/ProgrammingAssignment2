## The following functions are based on a "template" provided in 
## the Coursera R Programming course. The course is part of the 
## Data Science specialization offered by Johns Hopkins University.
## These functions are very similar to a set of functions provided
## in class. Where the example provided in class was based on a 
## Vector these functions are based on a Matrix. 
## 
## The two functions provide a mechanism to create an object 
## that stores a matrix and caches its inverse. The cache 
## mechanism is facilitated by assigning the matrix to
## an object in the parent environment via the <<- operator.

## This function, makeCacheMatrix, creates a special "matrix" 
## object that can cache its inverse. The matrix is  is really 
## a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {

  ## Initialize cache to NULL 
  inv <- NULL
  
  ## Define functions that will be put in a list
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setmatrix <- function(solve) inv <<- solve
  getmatrix <- function() inv
  
  ## Functions are defined now put them in the list 
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)

}


## This function, cacheSolve, computes the inverse of the special
## "matrix" returned by makeCacheMatrix above. If the inverse has
## already been calculated (and the matrix has not changed), then
## function will return the inverse from the cache.
cacheSolve <- function(x, ...) {
  
  ## Get the cached value
  inv <- x$getmatrix()
  
  ## Check the cached value and if its not NULL return it.
  if(!is.null(inv)) {
    message("getting cached matrix")
    return(inv)
  }
  
  ## Cache is NULL so create the inverse, cache it, and return it.
  data <- x$get()
  inv <- solve(data, ...)
  x$setmatrix(inv)
  inv
}
