## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a self describing object wich contains
## matrix and its inverse. Acordingly it has setter and getters for matrix and it's invers
## Additionally I implemented the function wich solve the inverse of the matrix. I don't now why I suspect it would be useful.

makeCacheMatrix <- function(x = matrix()) {
  #declaration of inverse of a matrix
  matrixinv <- NULL
  
  #setter - you can change the matrix in object then cache of inverse will be deleted
  set <- function(y){
    x <<- y
    matrixinv <<- NULL
  }
  #getter of matrix - get matrix which was written in the object
  get <- function() x
  #set cached matrix
  setmatrixinv <- function(minv) matrixinv <<- minv
  #get cached matrix
  getmatrixinv <- function() matrixinv
  
  solveinv <- function(){
    if(is.null(matrixinv)){
      matrixinv <<- solve(x)
      return(matrixinv)
    }
  }
  #pass "methods" outside the function
  list (set = set, get = get,
         setmatrixinv = setmatrixinv,
         getmatrixinv = getmatrixinv,
         solveinv = solveinv)
  
}


## Write a short comment describing this function
## This function solve the inverse of the matrix from the object created by the function above
## only if the inverse has not been already solved.
## If it he inverse was solved it will get cached data.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  #try if the inverse have been already counted
  matrixinv <-x$getmatrixinv()
  if(!is.null(matrixinv)){
    message("getting cached data")
    return(matrixinv)
  }
  # solve the inverse
  matrixinv<-x$solveinv()
  # and show the inverse
  matrixinv
}
