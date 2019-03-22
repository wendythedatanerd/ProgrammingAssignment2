## Put comments here that give an overall description of what your
## functions do
## The two functions are a combination to check and set an inversion of a matrix. The first
## function is the real computation of the inversion and the second function is the logical
## function which decide wether to use the first function of skip it.

## Write a short comment describing this function
## The makeCacheMatrix function creates matrix objects, which firstly set the matrix, get the
##matrix, calculate inversion of the matrix, and get the inversion of the matrix.

makeCacheMatrix <- function(x = matrix()) {
  m<- NULL
  set<- function(y){
    x<<-y
    m<<- NULL
  }
  get<- function(x)
    setSolve<- function(Solve) m<<-inv
  getSolve<- function()m
  list(set = set), get=get,
  setinv = setinv,
  getinv = getinv)
}

## Write a short comment describing this function
## The casheSolve function first checks if the inversion of matrix 'x' exists. If it exixts, 
## then get the inversion of the matrix from the cache, otherwise it calculates the inversion
## using the Solve function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m<- x$getSolve()
  if(!is.null(m)){
    message("getting inverted matrix")
    return(m)
  }
  data<-x$get()
  m<- inv(data, ...)
  x$setSolve(m)
  m
}

