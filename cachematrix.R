 ## Create a special matrix with four help funcs 
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  ##set & get for matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  ##set & get for reverse matrix
  setinverse <- function(solve) inv <<- solve
  getinverse <- function() inv
  ##return list of functions (meaning fields)
  list(set = set, 
       get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)
} 


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  ##if inv already exists -> return
  if(!is.null(inv)) return(inv)
  ##else get x, solve revers matrix and set it to x
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}
