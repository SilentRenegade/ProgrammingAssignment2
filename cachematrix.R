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

## cacheSolve will compute the inverse of the special matrix returned by
## makeCacheMatrix. If the inverse has already been calculated and the  
## matrix has not changed, then the cacheSolve should retrieve the      
## inverse from the cache.

cacheSolve <- function(x, ...) {
  inv <- x$getInverse() 
  if(!is.null(inv)) {
    message("Getting cached matrix...")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}
## To test this: 
## 1. Create a basic matrix such as: x <- matrix(1:4, nrow=2, ncol=2)
## 2. Then use the makeCacheMatrix with your matrix created in x: 
##   testmat <- makeCachMatrix(x)
## 3. To get the inverse matrix... use cacheSolve: sol <- cacheSolve(testmat)
## 4. Print sol
## 5. This will give a basic 2x2 matrix like so:
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## 6. To test the message received if the matrix has already been computed:
## sol2 <- cacheMatrix(testmat)