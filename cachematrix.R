## This two functions calculate the inverse matrix and avoid costly computations.
## The first function stores a "special" matrix which will be stored. Later 
## the second function will calculate the inverse matrix from the one stored.

## This function  helps to relationate a matrix given with a list of four 
## functions, so we can disposse from them later.

makeCacheMatrix <- function(x = matrix()) {
  inv<- NULL
  set<-function(y){
    x<<-y
    inv<<-NULL
  }
  get<-function()x
  setInverse<- function(inversemat){
    inv<<-inversemat
  }
  getInverse<- function()inv
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}

## This function will help to calculate the inverse matrix and to store it in 
## the cache. The if loop checks if the inverse is already stored and if it is
## true ends the function. If not, it calculates the inverse matrix. The for 
## loop calculated the identity matrix so we can calculate the inverse matrix. 

cacheSolve <- function(x, ...) {
  ## Assuming the given matrix is always invertible
  inv<-x$getInverse()
  if(!is.null(inv)){
    return(inv)
  }
  data<-x$get()
  a<-nrow(data)
  b<-ncol(data)
  z<-matrix(data=NA,a,b)
  for(i in 1:a){
    for(j in 1:b){
      if(i==j){
        z[i,j]=1
      }
      else{
        z[i,j]=0
      }
    }
  }
  inv<- solve(data,z)
  x$setInverse(inv)
  inv
}
