## Cacheable matrix inverse calculation

## Creates a special matrix that allows for caching its own inverse
makeCacheMatrix <- function(rawMatrix = matrix()) {
  cachedInverse <- NULL

  set <- function(updatedMatrix) {
    rawMatrix <<- updatedMatrix
    cachedInverse <<- NULL
  }

  get <- function() rawMatrix
  setInverse <- function(inverse) cachedInverse <<- inverse
  getInverse <- function() cachedInverse
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## Returns a matrix that is the inverse of 'x'
cacheSolve <- function(cacheMatrix, ...) {
  inverse <- cacheMatrix$getInverse()

  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }

  rawMatrix <- cacheMatrix$get()
  inverse <- solve(rawMatrix, ...)
  cacheMatrix$setInverse(inverse)
  return(inverse)
}
