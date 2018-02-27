##makeCacheMatrix creates a special matrix object to cache its inverse##

##As with the mean example, this will accomplish four objectives:
##1. Set the matrix
##2. Get the matrix
##3. Set the inverse of the matrix
##4. Get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y = matrix()) {
            x <<- y
            inv <<- NULL
    }
      get <- function() x
      setInverse <- function(inverse) inv <<- solve 
      getInverse <- function() inv
      list(set = set, get = get,
                setInverse = setInverse,
                getInverse = getInverse)
       
}      