## Put comments here that give an overall description of what your
## functions do

#makecachematrix: is a function that stores 4 different functions (getters and setters)
#to compute the matrix inverse with a second function (cachesolve)

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function (y) {
    x <<- y
    m <<- NULL
  }
  get <- function () x
  setinverse <- function(solve) m <<- solve
  getinverse <- function () m
  list (set = set, get = get, setinverse = setinverse ,getinverse = getinverse)
}


#CacheSove: complements the  makecachematrix and computes the inverse matrix of the
#given imput in a makecachematrix() form

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message ("getting cached data")
    return (m)
  }
  data <- x$get()
  m <- solve(data, ...) #
  x$setinverse(m)

  ## Return a matrix that is the inverse of 'x'
  m
}
