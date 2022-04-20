## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##We have to write code that caches the matrix inverse
## Write a short comment describing this function
##If a little more in detail, this function creates an object
##in which you can store our already cached inverse matrix
## and we use library mass in order to use function solve

library(MASS)
makeCacheMatrix <- function(hello = matrix()) {
  inVersE  <-  NULL  ##initialize inVersE as NULL;
  
  set  <-  function(f){
    
    hello  <<-  f
    inVersE <<-  NULL
  }
  get  <-  function() hello
  setINV  <-  function(invrs) inVersE <<- invrs ## -> inVersE in parent environment
  getINV  <-  function() inVersE  ## <- inVersE where called
  list(set = set, get = get, setINV = setINV, getINV = getINV)
}


## Write a short comment describing this function
## This function calculates the inverse of the matrix 
##returned by the function above.


cacheSolve <- function(hello, ...) {
        ## Return a matrix that is the inverse of 'hello'
  inVersE  <-  hello$getINV()
  if (!is.null(inVersE)){
    message("cashed matrix")
    return(inVersE)
  }
  d  <-  hello$get()
  inVersE  <-  solve(d, ...)
  hello$setINV(inVersE)
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
