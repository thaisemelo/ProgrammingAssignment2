## Programming Assignment 2: Lexical Scoping (11/08/2020)
##This code presents 2 functions: 
#The first one is :makeCacheMatrix, which creates a special "matrix" object that can cache its inverse.
#And cacheSolve, which computes the inverse of the special "matrix" returned by makeCacheMatrix above.
#If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

## creates a matrix that cache its inverse
makeCacheMatrix <- function(x = matrix()) {
m<-NULL
set<-function(y) {
  x<<-y
  m<<-NULL
}
get<-function () x
setInverse<-function(inverse) m<<-inverse
getInverse<-function() m
list(set=set, get=get,
     setInverse=setInverse,
     getInverse = getInverse)
}

##compute the inverse of the cached matrix
cacheSolve <- function(x, ...) {
  m<- x$getInverse()
  if(!is.null(m)){
    message("getting cache matrix")
    ## Return a matrix that is the inverse of 'x'
    return(m)
  }
  data<-x$get()
  m<-solve(data,...)
  x$setInverse(m)
  m
}
