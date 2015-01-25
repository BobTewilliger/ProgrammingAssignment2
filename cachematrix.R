#These functions allow the inversion of a matrix (via cacheSolve) created through makeCacheMatrix. 
#The result is stored in cache memoroy to avoid recalculation and allowsfaster operations 

#Creates a matrix and stores it in cache as 'x'. Creates a cache variable 'm' to store inverse matrix. 
#Inverts a matrix through 'solve' which is retrievable through getinverse()
makeCacheMatrix <- function(x = matrix()) { 
  m <- NULL 
  set <- function(y) { 
    x <<- y         
    m <<- NULL  
  }
  get <- function() x   
  setinverse <- function(solve) m <<- solve 
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

# Confirms if inverse has been called previously (!is.null) and prints 'm' (if NULL prints NULL).
# If m is empty inverse is calculated and returned to get$inverse (in addition to being stored 
# in cache as m) then printed.
cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}