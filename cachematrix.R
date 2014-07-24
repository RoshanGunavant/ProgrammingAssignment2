## makeCacheMatrix - Accepts a source matrix as a parameter and creates 
## a special "matrix" object/environment with methods to cache the 
## source and inverse matrices

## cacheSolve - returns inverse of source matrix from given matrix object
## either by retreiving it from cache or computing using solve function
## The source matrix needs to be square matrix 

## test. methods are the RUnit cases written to verify makeCacheMatrix
## and cacheSolve functions



## makeCacheMatrix function creates a matrix environment with list
## of getters and setters function to save and retreive source and
## inverse matrices.If input parameter is not passed, makeCacheMatrix
## creates an empty 1x1 matrix (NA)

makeCacheMatrix <- function(x = matrix()) {
  #Initialize inverse object to NULL
  inv <- NULL  			    
  
  # Setter function to set source matrix
  # Since we are creating matrix object, check if paramter is a matrix object
  set <- function(y) {
    if(class(y) == "matrix"){
      x <<- y
      inv <<- NULL          
    }
  }
  
  # Getter function to retreive source matrix
  get <- function() x
  
  # Setter function to save/cache inverse matrix
  setinv <- function(inverse) {
    # Check if "inverse" is of type matrix.
    if(class(inverse) == "matrix") {
      inv <<- inverse   
    }
  }   
  
  # Getter function to retreive inverse matrix
  getinv <- function() inv              
  
  # Define the list of getters and setters
  list(set = set, get = get,          
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve function takes the object/environment created using
## makeCacheMatrix as an input paramter. It fetches the inverse 
## matrix from the object cache if available. If not computes the
## inverse of source matrix using solve function.The computed inverse
## matrix if put back to object cache so that on next call it's
## retreived from cache.Function returns the inverse matrix

cacheSolve <- function(x, ...) {
  # Get the inverse matrix from environment "x"
  inv <- x$getinv()                     
  
  # Retreive the source matrix from environment "x"
  data <- x$get()  
  
  
  # (i)Check if inverse matrix was available in cache.
  # (ii)If inverse matrix was available, check if it is inverse of the matrix in x - 
  # NOTE : This is done by cheking that inverse(inverse matrix) == source matrix.
  # If x is source matrix and y is it's inverse then solve(y) = x.
  # Use identical function to compare data and inv. all.equals also can be used
  # print confirmation about the same and return the inverse matrix. Function execution terminated
  if(!is.null(inv) && identical(solve(data),inv) == T) {                   
    message("Retrieving inverse from cache")
    return(inv)                       
  }                     
  
  # Terminate execution if object does not contain a matrix of if matrix is not square
  if(class(data) != "matrix")
    stop("Error - Source Object not a matrix")
  else if(nrow(data) != ncol(data))
    stop("Error - Source matrix not square")
  
  # Compute the matrix inverse using solve function
  # Assume source matrix is square matrix
  inv <- solve(data)               
  
  #Save the matrix inverse computed above to environment "x" so that it's available from
  #cache from the next call to cacheSolve
  x$setinv(inv)                         
  
  # Return the matrix inverse
  inv
}






###Unit Test cases - Install RUnit package to run
## Test makeCacheMatrix getter functions.
test.makeCacheMatrixInitialization <- function(){
  x <- matrix(1:4,2,2)
  obj <- makeCacheMatrix(x)
  
  #Check if x is assigned properly and inverse is NULL (not computed)
  checkEquals(obj$get(),x)
  checkEquals(obj$getinv(),NULL)
}

## Test makeCacheMatrix getter functions. No Input paramter. 
test.makeCacheMatrixDefaultParameter <- function(){
  obj <- makeCacheMatrix()
  
  #Check if x is assigned to empty matrix and inverse is NULL (not computed)
  checkEquals(obj$get(),matrix(NA,1,1))
  checkEquals(obj$getinv(),NULL)
}

## Test makeCacheMatrix setter functions
test.makeCacheMatrixStoreSourceMatrix <- function(){
  x <- matrix(1:4,2,2)
  # Create obj with empty matrix
  obj <- makeCacheMatrix()
  checkEquals(obj$get(),matrix(NA,1,1))
  checkEquals(obj$getinv(),NULL)
  
  # use set function to set 2x2 matrix and verify using getter functions. Inverse is still NULL
  obj$set(x)
  checkEquals(obj$get(),x)
  checkEquals(obj$getinv(),NULL)
}

## Test makeCacheMatrix for caching/storing inverse matrix
test.makeCacheMatrixCacheInverseMatrix <- function(){
  x <- matrix(1:4,2,2)
  obj <- makeCacheMatrix(x)
  checkEquals(obj$getinv(),NULL)
  y <- solve(x)
  obj$setinv(y)
  checkEquals(obj$getinv(),y)
}

# Test cacheSolve function for inverse matrix
test.cacheSolveInverse <- function(){
  x <- matrix(1:4,2,2)
  obj <- makeCacheMatrix(x)
  inv <- matrix(c(-2,1,1.5,-0.5),2,2)
  # Check that inverse matrix computed using solve function matches with inv above
  checkEquals(cacheSolve(obj),inv)
}

# Test cacheSolve - Check that inverse of inverse matrix is source matrix
test.SolveInverseInverse <- function(){
  x <- matrix(1:4,2,2)
  inv <- solve(x)
  checkEquals(matrix(c(-2,1,1.5,-0.5),2,2),inv)
  
  y <- matrix(c(-2,1,1.5,-0.5),2,2)
  checkEquals(solve(y),x)  
}


# Test cacheSolve - Check when source matrix changes, cacheSolve inverse matrix differs
test.cacheSolveSourceMatrixChanged <- function(){
  x <- matrix(1:4,2,2)
  obj <- makeCacheMatrix(x)
  checkEquals(obj$get(),x)
  checkEquals(cacheSolve(obj),matrix(c(-2,1,1.5,-0.5),2,2))
  checkEquals(cacheSolve(obj),matrix(c(-2,1,1.5,-0.5),2,2))
  
  # Change source matrix
  obj$set(matrix(5:8,2,2))
  checkEquals(cacheSolve(obj),matrix(c(-4,3,3.5,-2.5),2,2))
}

## Test solve function exception when input matrix is not square
test.SolveNonSquareMatrix <- function(){
  x <- matrix(1:6,2,3)
  checkException(solve(x))
}

## Test cacheSolve function exception when input matrix is not square
test.cacheSolveNonSquareMatrix <- function(){
  x <- matrix(1:6,2,3)
  obj <- makeCacheMatrix(x)
  checkException(cacheSolve(obj))
}

## Test cacheSolve function exception when input is not a matrix
test.cacheSolveNonMatrix <- function(){
  y <- 10
  obj <- makeCacheMatrix(y)
  checkException(cacheSolve(obj))
}


