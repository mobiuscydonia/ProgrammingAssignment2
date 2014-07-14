## this first function will allow us to associate a variable with a set of functions that pertain to calculating the inverse of a matrix. This way
## the variable will have info in regards to whether or not an inverse has been calculated by checking the cached variables.

makeCacheMatrix <- function(x = matrix()) {
    m<-NULL
    
    set<-function(y){
      
      x<<-y
      m<<- NULL 
      
    }
    
    get <- function () x #return the input data (matrix)
    setinverse<-function(inverse) m<<-inverse #cache the gotten inverse term that comes in as input when we decide to call the set function inside the CacheSolve function.
    getinverse<-function() m
    
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse =getinverse)
    
  }
  

## This function will do the operation if the above created variable is empty of the inverse of itself.

cacheSolve<-function(x,...){
  
  m<-x&getinverse()
  
  if(!is.null(m))#if m is null that means that the computation hasn't been performed and no inverse has been assigned to the variable.
    {
    message("This computation has already been performed. To save time, we will return the previously cached version")
    return(m)}
  else
  {
    data<-x$get()
    m<-solve(data,...) #Actually calculate the inverse
    x$setinverse(m)
    return x$getinverse()
  }
  

}

