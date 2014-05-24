## Matrix inversion implementing a caching method for faster processing

## Creates a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  cache <- NULL                    # Initialize to NULL
  set <- function(newMatrix) {     # Set to a new matrix
    x <<- newMatrix
    cache <<- NULL
  }
  get <- function() x              # Get the original matrix
  setCache <- function(paraCache) cache <<- paraCache    # Set the cached value
  getCache <- function() cache     # Get the cached value
  list(set = set, get = get, setCache = setCache, getCache = getCache)
}


## Computes the inverse of a CacheMatrix. If the inverse is already
## in its cache, the cache is returned.
cacheSolve <- function(x, ...) {
  inverse <- x$getCache()
  if (!is.null(inverse)) {
    return(inverse)                # A cached value already exists, so return it
  }
  matrixdata <- x$get()   
  inverse <- solve(matrixdata)     # Compute the inverse of the matrix
  x$setCache(inverse)              # Store the inverse in the cache
  inverse                          # Return the inverse
}
