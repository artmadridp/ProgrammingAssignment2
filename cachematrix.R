## These functions allow the user to store the inverse of a matrix in cache for future use. They assume the matrix
## is square and solveable

## The first function creates a list of functions based on a matrix that can be called by the second function
## to either get or set the matrix contents and its inverse

makeCacheMatrix <- function(xm = matrix()) {
      mm=NULL
      setm<-function (ym){
            xm <<- ym
            mm <<- NULL
      }
      getm <- function() xm
      setinv <- function(inv) mm <<- inv
      getinv <- function() mm
      list(setm = setm, getm = getm,
           setinv = setinv,
           getinv = getinv)
}



## The second function takes an object of the type makeCacheMatrix and runs the most efficient function to 
## return the inverse of the matrix, depending on whether the data is stored in the cache or not

cacheSolve <- function(xm, ...) {
      mm <- xm$getinv()
      if(!is.null(mm)) {
            message("getting cached data")
            return(mm)
      }
      data <- xm$getm()
      mm <- solve(data,...)
      xm$setinv(mm)
      mm
}  
      
      ## Return a matrix that is the inverse of 'x'

