## In the following two functions a special object is created which stores a matrix and cache the inverse.
 
## The first function (makeCacheMatrix) is uded to create a special matrix (which is a list)
 
## the list actually contains functions to 
## set the value and get the value of the matrix
## set the value and get the value of the inverse  

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set_matrix <- function(y)
    {
    	 x <<- y
    	 i <<- NULL
    }
    
    get_matrix <- function() x
    
    set_inverse <- function(inverse) i <<- inverse
    get_inverse <- function() i
    
    list(set_matrix = set_matrix,
         get_matrix = get_matrix,
         set_inverse = set_inverse,
         get_inverse = get_inverse)
}



## The following function (cacheSolve) computes the inverse of thee matrix returned by the previous function
## Condition - if inverse has been calculated and not changed retrieve inverse from cache

cacheSolve <- function(x, ...) 
{
	
  ## Return a matrix that is the inverse of 'x'
  ## Assume the matrix is always invertible
  
  i <- x$get_inverse()
    if (!is.null(i))
      {
      	message("Getting Cached Data")
      	return(i)
      }
      
     data <- x$get_matrix()
     i <- solve(data, ...) 
     x$set_inverse(i)
     i
  
}


