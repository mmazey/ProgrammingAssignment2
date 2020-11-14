## These two functions can used to calculate the inverse of a matrix. They 
## demonstrate lexical scoping and the advantages of caching potentially 
## time-consuming processes. Example of using these functions would be:

## A <- matrix(c(1, 2, 3, 4), nrow = 2, ncol = 2)
## invA <- cacheSolve(makeCacheMatrix(A))
## invA

## In this example, invA is the inverse of matrix A. 
## As a check, invA %*% A = identity matrix


## This function creates a list of four functions that act on a matrix, 
## This function creates a special "matrix" object that can cache its inverse. 

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL # i (inverse) initialized to NULL
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) i <- inverse 
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve takes as input a special "matrix", which is an output from the makeCacheMatrix
## function, and it checks to see if the inverse of that "matrix"has already been 
## calculated (and not been changed), and then if not, calculates and retrieves it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data)
  x$setinv(i)
  i
}
