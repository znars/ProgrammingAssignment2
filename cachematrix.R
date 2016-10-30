## Put comments here that give an overall description of what your
## functions do

## This is a series of functions which will be benefit to caching the inverse of
## a matrix instead of computiong it repeatedly. It is very important in the algorithm 
## design since matrix inversion is usually a complex computation in time or memory space.

## Write a short comment describing this function

## This function is used to cache the inverse of a matrix, 
## which is really a list containing a function to
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse of the matrix
## get the value of the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  Inv <- NULL
  set <- function(y){
    x <<- y
    Inv <<- NULL
  }
  get <- function() x
  setInv <- function(inversematrix) Inv <<- inversematrix
  getInv <- function() Inv
  list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## Write a short comment describing this function

## This function is used to calculate the inverse of the matrix
## created with the function above. If the inverse of the matrix has not been calculated,
## and the matrix is reversible, it will calculates the inverse of the data and set the inverse in the cache
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  Inv <- x$getInv()
  if(!is.null(Inv)){
    message("getting cached data")
    return(Inv)
  }
  ## check whether it has been calculated
  data <- x$get()
  if(det(data)==0){
    message("The matrix is irreversible")
    return(Inv)
  }
  ## determine whether it is reversible
  Inv <- solve(data,...)
  x$setInv(Inv)
  Inv
}
