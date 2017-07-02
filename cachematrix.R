## R Programming
## Programming Assignment 2
## L.C., July 2nd

#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#
#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#
#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#@#

# Our purpose is to reduce the run time of any program in which we need to
# invert a given matrix multiple times (for example, when the inversion is 
# performed inside a loop). Instead of computing the inverse each time using
# the solve() function, we compute it only once and then cache it and retrieve
# it whenever needed. This saves a lot of computing time, especially with
# large matrixes or when a matrix has to been inverted lots and lots of times.
# We will write two functions, to create a special object that stores
# a matrix and allows to cache and retrieve its inverse.

# The first function creates a list that contains 4 functions. These functions
# allow to:
# 1. change the stored matrix
# 2. get the stored matrix
# 3. cache the inverse
# 4. retrieve the inverse
# The input is a matrix and the output is a list.
# The output can also be seen as a special object that contains the inputted
# matrix and allows to cache and retrieve its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(invmatrix) inverse <<- invmatrix
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

# The second function returns the inverse of a matrix.
# The input is a matrix that was "transformed" into a special object using
# the function above.
# If, for a given matrix, the inverse has already been calculated, the
# function will return the inverse by retrieving it from the cache (and let us
# know that it has returned the cached data).
# If the function is applied for the first time to an object (or the matrix 
# contained in an object has been changed), the function will compute the
# inverse, cache it for future use and then return it.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()
  if (!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}

