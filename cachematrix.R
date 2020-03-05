## here are two functions in this file. They try to cache and retrieve the
## inverse of a matrix to save time in computation. The function structure is based 
## on the given example and modified according to the different application purpose.



## The makeCacheMatrix function aims to create a special matrix object to be able to 
## cache the inverse of that matrix. It creates a list of functions to set and get the
## matrix and also set and get its inverse. The "<<-" operator makes it possible to
## use the function in different matrix (just like different environment) as it could 
## assign a value to an object that is not in the current environment.  

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set <-function(y){
    x<<-y
    inv<<-NULL
  }
  get<- function ()x
  setinv<-function(inverse) inv <<-inverse
  getinv<-function()inv
  list(set=set, get=get,
       setinv=setinv,
       getinv=getinv)
}


## This cacheSolve function is to get the inverse of the matrix created by the first
## function. It will first try to find the inverse in the cache and return it if it
## is already calculated. If not, it will calculate the inverse with the solve function
## and store it in the cache. 


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv<-x$getinv()
  if(!is.null(inv)){
    message("getting chached data")
    return(inv)
      }
    data <-x$get()
    inv<-solve(data,...)
    x$setinv(inv)
    inv
}
