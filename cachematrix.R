makeCacheMatrix <- function(x = matrix()){
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  ##creates the special matrix
  
  get <- function() x
  setmatrix <- function(solve) m <<- solve
  getmatrix <- function() m
  list (set = set, get = get,
        setmatrix = setmatrix,
        getmatrix = getmatrix)
}

cacheSolve <- function(x=matrix(), ...){
  m <- x$getmatrix()
  if (!is.null(m)){
    message("getting cached data")
    return(m)
  }
  matrix <- x$get()
  m <- solve(matrix, ...)
  x$setmatrix(m)
  m
}
##computes the inverse of the special matrix

a <- makeCacheMatrix()
## Store a function in a variable a

a$set(matrix(1:4,2,2))
## stores a matrix

cacheSolve(a)
## provides result
