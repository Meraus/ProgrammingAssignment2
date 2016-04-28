## Assignment 2

### ASSIGNMENT ###


# Write the following functions:

# 1.makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {            
    x <<- y
    m <<- NULL
  }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,    # stores the 4 functions
         setinverse = setinverse,
         getinverse = getinverse)
}



a <-  matrix(c(1, 3, 17, 35, 3, 7, 2, 5, 1, 5, 13, 43, 1, 6, 12, 33), nrow = 4, ncol = 4)
a
inverse <- solve(a)
inverse


# 2.cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.


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

cacheSolve(makeCacheMatrix(a))


