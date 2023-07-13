## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##There are two functions makeCacheMatrix, makecacheMatrix
##makeCacheMatrix consists of set, get, setiny, getinv 
##library (MASS) is used to calculate inverse for non squared as well as square matrices
library(MASS)
makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set<-function (y){
                   x<<-y 
                   inv<<-NULL
                   }
  get<-function ()x            #function to get matrix x
  setinv<-function(inverse)inv<<-inverse 
  getinv<-function() {
                      inver<-ginv(x) 
                      inver%*%x        #function to obtain inverse of the matrix
                      } 
  list(set = set, get = get, 
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function
##This is used to get the cache data

cacheSolve <- function(x, ...) ##gets cache data
  {
  inv<-xsgetinv()  
  if (!is.null(inv)) {                #checking whether inverse is NU11
                       message ("getting cached data!") 
                       return(inv)            #returns inverse 
    }
  data<-x$get() 
  inv<-solve (data,...)   #calculates inverse value 
  x$setinv(inv)
  inv ## Return a matrix that is the inverse of 'x'
}
