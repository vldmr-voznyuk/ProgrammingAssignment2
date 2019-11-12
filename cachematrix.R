
# The below functions are created to avoid costly re-computing of the inverse
# of a matrix. 

# Function "makeCacheMatrix" creates a list of funcions to: 
# 1.set the value of the matrix
# 2.get the value of the matrix
# 3.set the value of the inverse matrix (uses a built-in solve() function)
# 4.get the value of the inverse matrix
# Set functions from the list uses <<- operator to store values in the parent function enviroment.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


# The "cacheSolve" function takes the output of "makeCacheMatrix" as an argument. It checks 
# if the inverse of the matrix was stored in cache, and either takes the result from cache or
# calculates the inverse of the matrix and stores it there.

# If "makeCacheMatrix" is called with a matrix as an argument, the inverse of that matrix is
# calculated. If the "makeCacheMatrix" is called without an argument (or for calculating the 
# inverse of a new matrix), the "set" function from the list should be called before executing
# the "cacheSolve" function. Please, see use-examples below. 

# NOTE: it is assumed the matrix supplied is always invertible. 

# Example 1. ---------------
# > mx<-makeCacheMatrix()
# > mx$set(matrix(c(1:4),2,2))
# > cacheSolve(mx)

# Example 2. ---------------
# > mx<-cacheSolve( makeCacheMatrix( matrix( c(1:4),2,2) ) )

# Now, when the inverse of the same matrix is needed again, the "cacheSolve" function 
# may be called or the $getinverse function used. In both cases the cashed value will be used.
# > cacheSolve(mx) 
# > mx$getinverse()

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
  # Return a matrix that is the inverse of 'x'
}