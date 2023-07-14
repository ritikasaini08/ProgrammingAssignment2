## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##There are two functions makeCacheMatrix, makecacheMatrix
##makeCacheMatrix consists of set, get 
makeCacheMatrix <- function(x = matrix(sample(1:100,9),3,3)) {
  inv<-NULL
  set<-function (y){
                   x<<-y 
                   inv<<-NULL
                   }
  get<-function ()x            #function to get matrix x
  setsolve <- function(solve) s <<- solve 
  list(set = set, get = get,
       setsolve = setsolve, 
       getsolve = getsolve)       #function to obtain inverse of the matrix
 } 
  



## Write a short comment describing this function
##This is used to get the cache data

cacheSolve <- function(x, ...) ##gets cache data
  {
  s <- x$getsolve()  
  if(!is.null(s)) {                #checking whether inverse is NU11
    message("getting inversed matrix") 
    return(s)            #returns inverse 
    }
  data <- x$get() 
  s <- solve(data, ...)   #calculates inverse value 
  x$setsolve(s)
  s ## Return a matrix that is the inverse of 'x'
}
