## Caching the matrix entered by user and inverse matrix when calculated in cacheSolve function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Calculating the inverse matrix when there is no cache or just getting the cached data when prior calculated

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  matrix_to_invert <- x$get()
  inv <- solve(matrix_to_invert, ...)
  x$setinverse(inv)
  inv
}

## Checking

chk_Matrix <- makeCacheMatrix(matrix(5:8, 2, 2))

chk_Matrix$get()
chk_Matrix$getinverse()
cacheSolve(chk_Matrix)


