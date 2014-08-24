## Use cache to save inverse of matrix
## 
## From Coursera R Programming HW2
##
## Functions
##   makeCacheMatrix(x=matrix())
##   cacheSolve(x,...)
##
## Example
## > source('cachematrix.R')   ## source the functions
## > d <- matrix(rnorm(9),3,3) ## define a matrix
## > d2 <- makeCacheMatrix(d)  ## cache the matrix
## > dinv <- cacheSolve(d2)    ## performs inverse and sets inverse in cache
## > dinv <- cacheSolve(d2)    ## gets the inverse of the matrix from the cache
## > d <- matrix(rnorm(9),3,3) ## define new matrix
## > d2 <- makeCacheMatrix(d)  ## cache the matrix
## > dinv <- cacheSolve(d2)    ## perform inverse of new matrix

## cache the matrix and define function which may operate on cached matrix
makeCacheMatrix <- function(x = matrix()) {
   ## initialize matrix inverse to NULL
   m <- NULL
   
   ## definie functions
   set <- function() {
           x <<- y
           m <<- NULL
   }
   get <- function() x
   setInverse <- function(solve) m <<- solve
   getInverse <- function() m
   
   ## construct list of functions
   list(set = set, get = get,setInverse=setInverse,getInverse=getInverse)
}

## perform the inverse of the matrix if the cached inverse is NULL, 
## otherwise return cached inverse
cacheSolve <- function(x, ...) {
   ## Return a matrix that is the inverse of 'x
   m <- x$getInverse()
   if(!is.null(m)){
           message("getting cached inverse")
           return(m)
   }
   matrix<-x$get()
   m <- solve(matrix, ...)
   x$setInverse(m)
   m
}


