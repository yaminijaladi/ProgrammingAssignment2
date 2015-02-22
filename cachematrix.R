## In this program, we create a "makeCacheMatrix" to create 
##a special matrix object and "cacheSolve" which computes inverse of the matrix.
##If inverse of matrix was done already then,
##it will search in cache and returns it, without 
##computing again.



makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL 
  }
  
  get <- function() x 
  setmatrixInv <- function(inv) m <<- inv 
  getmatrixInv <- function() m 
  list(set = set, get = get,
       setmatrixInv = setmatrixInv,
       getmatrixInv = getmatrixInv)

}


## Here "cacheSolve" computes inverse of special matrix
## that was returned by "makeCacheMatrix". 
##If inverse already exists then it fetches from the cache.


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getmatrixInv() 
  
  if(!is.null(m)) { 
    message("getting cached inverse matrix")
    return(m) 
  }
  data <- x$get() 
  m <- solve(data) 
  x$setmatrixInv(m) 
  m 
        
}
