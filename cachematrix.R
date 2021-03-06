## Create a special list with four help funcs 
# set (value of the matrix)
# get (...)  If the matrix isn`t square -> execution error in cacheSolve
# set (value of the inverse of matrix)
# get (...)
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  #set & get for matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  #set & get for reverse matrix
  setinverse <- function(solve) inv <<- solve
  getinverse <- function() inv
  #return list of functions (meaning fields)
  list(set = set, 
       get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)
} 

#cacheSolve function calculates the inverse of the special "vector" created by makeCacheMatrix

cacheSolve <- function(x, ...) {
  # Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  #if inv already exists -> return
  if(!is.null(inv)) return(inv)
  #else get x, solve revers matrix and set it to x
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}
