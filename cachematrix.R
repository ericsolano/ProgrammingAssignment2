
####################################################################################################
#
#           Assignment 2
#   
#     This project includes 2 functions that compute and cache the inverse of a matrix.
#     Function makeCacheMatrix takes a matrix as a parameter and returns a list of four 
#                objects, where each object is a function itself.
#     Function cacheSolve takes an object of type 'makeCacheMatrix' and returns either a   
#                        newly computed inverse of a matrix or the cached inverse of a matrix.
#
###################################################################################################



#---------------------------------------------------------------------------------------------------
# Function makeCacheMatrix creates a special object, which is really a list with four objects.
# It takes a matrix as the argument.
# The returned object is a list with four functions: 
#           - set function to set the matrix 
#           - get function to get the matrix
#           - setInverse to set the inverse of the matrix
#           - getInverse to get the inverse of the matrix
#---------------------------------------------------------------------------------------------------

makeCacheMatrix <- function(x = matrix()) {

      inv <- NULL
      set <- function(y){
          x <<- y
          inv <<- NULL
      }
      get <- function() x
      
      setInverse <- function(inverse) inv <<- inverse
      getInverse <- function() inv
      
      list(set = set, get = get,
           setInverse = setInverse,
           getInverse = getInverse)
  
  
}


#---------------------------------------------------------------------------------------------------
# Function cacheSolve creates a special object, which is really a list with four objects.
# It takes an object of type 'makeCacheMatrix' as the argument.
# First it checks if the inverse of the matrix had been cached before, and if it was, it returns it. 
# If the inverse had not been computed before, it prepares to compute it. 
#          The next step is to extract the matrix object stored in 'makeCacheMatrix'. 
#          Then, it computes the inverse by:
#             a) using function solve() if the matrix is square    
#             b) using function ginv() from package MASS if the matrix is not square
#  Finally, it sets and caches the computed inverse value in the 'makeCacheMatrix' object.
#
#---------------------------------------------------------------------------------------------------

cacheSolve <- function(x, ...) {
  
      library(MASS)
  
      ## Return a matrix that is the inverse of 'x'
      inv <- x$getInverse()
      
      if(!is.null(inv)) {
        message("Getting cached data...")
        return(inv)
      }
      
      data <- x$get()
      
      ##check matrix dimensions
      
      ###use the solve() function if square matrix
      if ( dim(data)[1]==dim(data)[2] ){
          message("Inverse of square matrix...")
          inv <- solve(data)
      }
      else { ##use Moore-Penrose Generalized Inverse of matrix
          message("Inverse of non-square matrix...")
          inv <- ginv(data)
      }

      x$setInverse(inv)
      
      inv
      
}
