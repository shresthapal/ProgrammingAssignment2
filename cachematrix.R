## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function - makeCacheMatrix creates a matrix and stores it's inverse value

makeCacheMatrix <- function(x = matrix()) {
  invx <- NULL
  set <- function(y) {       
    x <<- y
    invx <<- NULL
  }
  get <- function() x        
  setinvx <- function(inverse) invx <<- inverse
  getinvx <- function() invx
  list(set = set, get = get,
       setinvx = setinvx,
       getinvx = getinvx)
}

## Write a short comment describing this function - cacheSolve computes the inverse of the matrix and fetches the inverse value from above if the inverse is present already

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invx <- x$getinvx()
  if(!is.null(invx)) {
    message("getting cached data")
    return(invx)
  }
  data <- x$get()
  invx <- solve(data, ...)
  x$setinvx(invx)
  invx
}
