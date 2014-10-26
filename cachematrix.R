## Put comments here that give an overall description of what your
## functions do

##rprog-008 ProgrammingAssignment2 - cachematrix

## This function returns a list with references to all 4 functions to:
## read and write a new matrix through the list;
## read and write the inverse matrix calculated a priori with cacheSolve
## Throws: Message when set(matrix) = not inversible

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      
      set <- function(y = matrix()) {
            inv <- NULL
            x   <<- y
            inv <<- NULL
            
            if (det(y) == 0) {
                  message("Warning in matrix$set : inverse of given matrix is not defined(not inversible).")
            }
            
      }
      
      get <- function() { 
            return(x)
      }
      
      setinv <- function (inverse) {
            inv <<- inverse
      }
      getinv <- function() {
            return(inv)
      }
      
      list(set = set, get= get,
           setinverse = setinv, getinverse = getinv)
}


## This function reads the matrix from the given 'special' matrix
## and proves if it has changed using the inverse;
## simultaneously proves if there is an inverse in Cache
## returns it and if there is non, it calculates it and puts it in Cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
      
      inv <- x$getinv()
      matrix<- x$get()
      if(!is.null(inv) && solve(matrix) == inv) {
            message("Matrix not changed and Inverse in Cache, getting inverse...")
            return(inv)
      }
      else {
            message("Computing inverse-matrix")
            inv <- solve(matrix,...)
            x$setinv(inv)
            return(inv)
      }
      
}
