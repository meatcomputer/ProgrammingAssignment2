## Create a list of functions then leverage said functions
## to cache matrix inverses (solve)

## Creates a list which is used to cache the inverse of a matrix once computed

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y){
    x <<- y
    s <<- NULL 
  }
  get <- function() x
  setSolve <- function(solve) s <<- solve
  getSolve <- function() s
  list(set = set,
       get = get,
       setSolve = setSolve,
       getSolve = getSolve)
}


## Returns solve(x). 
## Returns from cache if it has been previously computed.

cacheSolve <- function(x, ...) {
        s <- x$getSolve()
        if(!is.null(s)) {
          message("getting cached data")
          s
        }
        temp <- x$get()
        s <- solve(temp)
        x$setSolve(s)
        s       
}

## function to create square matrices for testing purposes

rmatrix <- function(dim) matrix(rnorm(dim^2),dim, dim)
