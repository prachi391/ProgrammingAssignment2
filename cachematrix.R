## makeCacheMatrix makes a special matrix object and caches the inverse of the matrix
## cacheSolve computes the inverse of matrix it gets from makeCacheMatrix.It checks if the inverse value is already present

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
Inv<- NULL
set<- function(y)
{
  x<<-y
  Inv<<-NULL
}
get<- function()x
setInv<-function(Inverse) Inv<<- Inverse
getInv<- function()Inv
list(set=set,get=get,setInv=setInv,getInv=getInv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        Inv<-x$getInv()
        if(!is.null(Inv))
        {
          message("Getting Cached Data")
          return(Inv)
        }
        data<- x$get()
        Inv<- solve(data,...)
        x$setInv(Inv)
        Inv
}
