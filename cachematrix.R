
## Matrix Inverse storage function:
## Matrix inversion is generally a difficult calculation,
## making it useful to cache the inverse of an matrix 
## instead of calculating it every time.
## The functions below create a way to 
## store an array and its reverse cache.
## The first function creates a special "matrix" 
## object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y) {
      x <<- y
      inv <<- NULL
     }
      get <- function() x
      setInverse <- function(inverse) inv <<- inverse
      getInverse <- function() inv
      list(set = set,
           get = get,
           setInverse = setInverse,
           getInverse = getInverse)
}


## The second function calculates the inverse
## of the special "matrix" created in "makeCacheMatrix" 
##in the previous function. 
##If the inverse matrix has already been calculated 
##(from a matrix that has not changed) this function will 
##recover the inverse of the calculated matrix in cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}
