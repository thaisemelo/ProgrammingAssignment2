## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## You have two functions here. On the first one (makCacheMatrix) you cache a matrix values. In the second one
## (cacheSolve) you solve the inverse of your cache matrix. For the issue about solving unsquared matrix 
## the MASS package is used. 

library(MASS)
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL #Assignments working in the current level 
    set <- function(y){
        x <<- y # Assignments workig in the parent level
        inv <<- NULL #Setting inverse as NULL
    }
    get <- function(){x} #getting matrix x
    setInverse <- function(inverse){
        inv <<- inverse
        }
    getInverse <- function(){
        inverso <- ginv(x)
        inverso%*%x
        } #getting inverse of the matrix
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        if(!is.null(inv)){ #cheks if inverse is null
            message("getting cached data")
            return(inv) #returning the inverse value
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}
