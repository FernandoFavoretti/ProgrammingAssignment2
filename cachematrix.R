##These two functions are able to reduce potentially time-consuming
##by caching the inverse of a matrix.


## This function prepare a list containing a function to:
## 1.set the value of the matrix
## 2.get the value of the matrix
## 3.set the value of the solve
## 4.get the value of the solve
makeCacheMatrix <- function(x = matrix()) {
  ## sets the value of m to NULL for default if cacheSolve
  ## has not yet been used.
  m<-NULL
  ##1.set the value of the matrix
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  ##2.get the values of the matrix
  get<-function() x
  ##3.set the value of the solve
  setmatrix<-function(solve) m<<- solve
  ##4.get the value of the solve
  getmatrix<-function() m
  ##create a list containing de sets and gets
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}


## Calculates the inverse of a matrix, with some
## checks (explained later) and assumes that the
## matrix is always invertible

cacheSolve <- function(x, ...) {
  ## if an inverse has already calculated get this
  ## and return the cached matrix
  m<-x$getmatrix()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  
  ##else, if an inverse has not yet been calculated
  ## get the inputed matrix and compute the value of
  ## the inverse of it
  matrix<-x$get()
  m<-solve(matrix, ...)
  ##cache de inverse calculated
  x$setmatrix(m)
  ##return the inverse
  m
}