## Our purpose in this file is to generate an upper layer of code in order
## to save computing time when calculating inverses of matrices. To do
## that we cache the results of previously computed inverses. A test case
## is also provided.

## This function generates a layer in-between the code of the program
## that the user is running and the actual computations taking place in
## the background.

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) inverse <<- inv
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The cacheSolve function is used to compute the inverse of the matrix
## for and makeCacheMatrix type of obect instead of the normal solve()
## function. Its main purpose is storing the precomputed inverse to
## avoid wasting computing resources.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached inverse")
    return(inv)
  }
  
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}

test <- function() {
  y <- matrix(c(2,3,-5,-8), nrow = 2, ncol = 2)
  x <- makeCacheMatrix(y)
  cacheSolve(x)
  cacheSolve(x)
  
  a <- matrix(c(1,3,-8,-8), nrow = 2, ncol = 2)
  b <- makeCacheMatrix(a)
  cacheSolve(b)
  cacheSolve(b)
}
