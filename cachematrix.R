## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##We have to write code that caches the matrix inverse
## Write a short comment describing this function
##If a little more in detail, this function creates an object
##in which you can store our already cached inverse matrix
## and we use library mass in order to use function solve

library(MASS)
makeCacheMatrix <- function(x = matrix()) {
  inVersE  <-  NULL
  set  <-  function(f){
    x  <<-  f
    inv  <<-  NULL
  }
  get  <-  function() x
  setINV  <-  function(inverse) inVersE <<- inverse
  getINV  <-  function() inVersE
  list(set = set,
       get = get,
       setINV = setINV,
       getINV = getINV)
}


## Write a short comment describing this function
## This function calculates the inverse of the matrix 
##returned by the function above.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inVersE  <-  x$getINV()
  if (!is.null(inVersE)){
    message("cashed matrix")
    return(inVersE)
  }
  d  <-  x$get()
  inVersE  <-  solve(d, ...)
  x$setINV(inVersE)
  inVersE    
}

##So , here what i get:
##adilka <- makeCacheMatrix(matrix(2:5, 2, 2))
##adilka$get()
##      [,1] [,2]
##[1,]    2    4
##[2,]    3    5
##cacheSolve(adilka)
##      [,1] [,2]
##[1,] -2.5    2
##[2,]  1.5   -1
