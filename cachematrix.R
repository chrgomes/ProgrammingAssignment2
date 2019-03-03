## Put comments here that give an overall description of what your
## functions do.

## Write a short comment describing this function.

## Caching the Inverse of a Matrix that uses solve() to 
## calculate the inverse of a given matrix and by 
## taking advantage of scoping rules is able to cache 
## the results. 

makeCacheMatrix <- function(orig.mat = matrix()) {

  ## Check correct input.
  
  if (!is.matrix(orig.mat)) {
    stop("Provide values for the matrix")
  }
  
  inv.mat <- NULL
  
  ## Set the value of the matrix.
  
  set.mat <- function(y) {
    
    orig.mat <<- y
    inv.mat <<- NULL
    
  }
  
  ## Get the value of the matrix.
  
  get.mat <- function() orig.mat
  
  ## Set the value of the inverse of the matrix.
  
  set.inverse <- function(solve) inv.mat <<- solve
  
  ## Get the value of the inverse of the matrix.
  
  get.inverse <- function() inv.mat
  
  ## Return the list.
  
  list(set.mat = set.mat, get.mat = get.mat, set.inverse = set.inverse, get.inverse = get.inverse)

}

## Write a short comment describing this function.

## Computes the inverse of the cacheable matrix returned 
## by makeCacheMatrix() If the inverse has already been 
## calculated and there's no change in the matrix
## then the cacheSolve() returns the cached inverse.

cacheSolve <- function(cacheable.mat, ...) {
  
  ## Return a matrix that is the inverse of "cacheable.mat".
  
  inv.mat <- cacheable.mat$get.inverse() ## Retrieve from cache.
  
  ## Is there a cached matrix available?
  
  if (!is.null(inv.mat)){
    message("Getting cached inverse matrix")
    return(inv.mat)
  }
  
  matrix.inv <- cacheable.mat$get.mat() ##Get the Inverse.
  
  inv.mat <- solve(matrix.inv) ## Solve the Inverse.
  
  cacheable.mat$set.inverse(inv.mat) ## Set the Inverse.
  
  inv.mat #Return the Inverse.
  
}
