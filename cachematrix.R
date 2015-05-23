## Together these functions compute the inversion of a matrix. Cashing is used to reduce computational load. Input of a matrix which can be inverted is required.

## makeCacheMatrix creates a special matrix for use in cacheSolve in preperation for inversion

makeCacheMatrix <- function(m = matrix()) {
  i <- NULL
  set  <- function(n){
      m <<- n
      i <<- NULL
  }
  get <- function() m
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  list(set = set, get = get,
     setinv = setinv,
     getinv = getinv)
}


## cacheSolve utilizes a cache system to save completed inversions, so they can be recalled without reanilyzation in further analysis

cacheSolve <- function(m, ...) {
  i <- m$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- m$get()
  i <- solve(data, ...)
  m$setinv(i)
  i
}

## Return a matrix that is the inverse of 'm'
