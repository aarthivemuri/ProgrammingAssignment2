## The functions below can be used to cache matrix inverse thereby saving the computational cost and time.
## If contents of the vectors are not changing it makes sense to retrive the maxtrix inverse from the cache rather than
## computing it again. 

##  This function creates a special "matrix" object that can cache its inverse in following steps:
## Set the value of the vector
## Get the value of the vector
## Set the value of the inverse
## Get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
      minv <- NULL
      set <- function(y) {
            x <<- y
            minv <<- NULL
      }
      get <- function() x
      setinv <- function(solve) minv <<- solve
      getinv <- function() minv
      list(set = set, 
           get = get,
           setinv = setinv,
           getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix funtcion.
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
## retrieves the inverse from the cache

cacheSolve <- function(x, ...) {
        minv <- x$getinv()
      if(!is.null(minv)) {
            message("getting cached data")
            return(minv)
      }
      matrix <- x$get()
      minv <- solve(matrix, ...)
      x$setinv(minv)
      minv
}
