## This function is actually a list of functions that does 4 things
## set the value of the matrix
## get the vcalue of the matrix
## get the inverse of the matrix
## set the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {                       #accepts a matrix input

  inverse_matrix <- NULL                                          #sets the first value of the matrix
  set_matrix <- function(y) {
    x <<- y
    inverse_matrix <<- NULL
  }
  get_matrix <- function() x                                     #gets the matrix value
  set_inverse <- function(inverse) inverse_matrix <<- inverse    #sets the inverse of the matrix
  get_inverse <- function() inverse_matrix                       #gets the inverse of the matrix
  list(set_matrix = set_matrix, get_matrix = get_matrix,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
  
}


## THis function calculates the inverse of the vector created by the list of functions above, 
## if the inverse of the matrix has been alredy calculated then it will return the result from the cache.

cacheSolve <- function(x, ...) {
        ## Return the inverse of 'x'
  inverse_matrix <- x$get_inverse()
  if(!isnull(inverse_matrix)){
    print("retirving cached information")
    inverse_matrix
  }
  matrix <- x$get_matrix()
  inverse_matrix <- solve(matrix,...)
  x$set_inverse(inverse_matrix)
  inverse_matrix
}
