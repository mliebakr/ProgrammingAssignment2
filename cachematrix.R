## makeCacheMatrix caches a matrix and the inverse of the matrix
## cacheSolve finds the inverse of a matrics
## f_ is used as prefix for additional functions to make code more readable

## The makeCacheMatrix function creates a list to store matrixes and the inverse of that matrix
## Functions are created to: 
##  1. set the value of the matrix
##  2. get the value of the matrix
##  3. set the value of the inverse matrix
##  4. get the value of the inverse matrix        

makeCacheMatrix <- function(x = matrix()) {
  StoredInverseMatrix <- NULL
  
  f_set <- function(NewMatrix) 
  {
    x <<- NewMatrix
    StoredInverseMatrix <<- NULL
  }
  
  f_get <- function () 
  {
   x 
  }
  
  f_SetInverseMatrix <- function (InverseValue)
  {
    StoredInverseMatrix <<- InverseValue
  }
  
  f_GetInverseMatrix <- function ()
  {
    StoredInverseMatrix
  }
  
  list (
    set = f_set,
    get = f_get,
    setmatrix = f_SetInverseMatrix,
    getmatrix = f_GetInverseMatrix
  )
}

## The cacheSolve function checks if the inverse of the matrix already is stored in the makeCacheMatrix
## function. 
## If the matrix is not cashed in makeCacheMatrix, the inverse of the matrix is calculated 
## and then stored using the function makeCacheMatrix.
## The value of the inverse matrix is returned

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

    InverseMatrix <- x$getmatrix()

    if (!is.null(InverseMatrix))
  {
    message("getting cached data")
    return(InverseMatrix)
  }
      
  data <- x$get()
  InverseMatrix <- solve(data, ...)
  x$setmatrix (InverseMatrix)
  InverseMatrix
}
