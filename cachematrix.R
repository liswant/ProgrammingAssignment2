## Put comments here that give an overall description of what your
## functions do

## || Usually, to obtain the inverse of a large matrix, it requires long computations.
## || Therefore, this function was created to optimize these processes, allowing its 
## || application in complex data or looped matrices more easily.

## Write a short comment describing this function

## || First, this function creates a matrix object ("makeCacheMatrix") that will cache its elements and its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL                              
  set <- function(y) {      ## set the elements of the matrix;           
    x <<- y                             
    inv <<- NULL    
  }
  get <- function() x      ## get the elements of the matrix;
  
  setinverse <- function(inverse) inv <<- inverse  ## 'setinverse' set the elements of the matrix inverse;
  getinverse <- function() inv   ## 'getinverse' get the elements of the matrix inverse. 
  list(set = set, get = get,      
       setinverse = setinverse,   
       getinverse = getinverse)   
}

## Write a short comment describing this function

## || Next, this function computates the inverse of the matrix  returned by the "makeCacheMatrix" object. 
## || If its inverse has already been computated, the function returns the data from the cache,
## || avoiding unnecessary computations.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  inv <- x$getinverse()              ## checks if the inverse has been already computated:
  if(!is.null(inv)) {
    message("getting cached data")   ## >> if so, it returns the data from the cache and skips computation;
    return(inv)
  }
  matrix_to_invert <- x$get()          ## >> if not, this function computates the inverse of the matrix;
  inv <- solve(matrix_to_invert, ...)  
  x$setinverse(inv)  ## set the newly computated inverse matrix in the cache.
  inv
}

##  ||  Instructions for the user (console):
##  ||    1) Define the desired matrix: 
##  ||      "> <matrix_name> <- makeCacheMatrix(matrix(<data>, <nrow>, <ncol>))"
##  ||
##  ||    2) Find out if the matrix inverse was already computated:
##  ||      "> <matrix_name>$getinverse()"
##  ||
##  ||    3) If not, it will print "NULL". Then, enter the code below:
##  ||      "> <matrix_name>$getinverse()"
##  ||
##  ||    ... and voilà! :)