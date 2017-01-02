

makeCacheMatrix <- function(x = matrix()) {
  z <- NULL
  set <- function(y) {
    x <<- y
    z <<- NULL
  }
  get <- function() x
  set_inv <- function(solve) z <<- solve
  get_inv <- function() z
  list(set = set, get = get,
       set_inv = set_inv,
       get_inv = get_inv)
}




cacheSolve <- function(x, ...) {
  z <- x$get_inv()
  if(!is.null(z)) {
    message("getting cached data")
    return(z)
  }
  data <- x$get()
  z <- solve(data, ...)
  x$set_inv(z)
  z
}


