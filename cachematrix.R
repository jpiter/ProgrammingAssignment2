## Cachematrix.R contains two functions that allow one to store an 
## inverse of a given matrix in memory and cache its value when needed
## rather than recompute it. This would save a program runtime when it
## is required to compute an inverse of a large matrix many times.

## makeCacheMatrix returns a list of functions:
## getm() - returns the original matrix
## setm() - stores original matrix in memory
## get_inv() - returns cached value of the inverse 
## set_inv() - stores the value of the inverse in memory
makeCacheMatrix <- function(x = matrix()) {
  
  m_inv<-NULL
  
  getm<-function() x
  
  setm<-function(y) {
    x<<-y
    m_inv<<-NULL
  
  }
  
  set_inv<-function(inv) m_inv<<-inv
  
  get_inv<-function() m_inv
  
  #returns a list of functions
  list(getm=getm, setm=setm, set_inv=set_inv, get_inv=get_inv)

}


## cacheSolve returns the value of the inverse for the given matrix object. 
## To work properly the argument must be a result of makeCacheMatrix. 
## The function computes the inverse if called for the first time, and returns
## cached value on subsequent attempts.

cacheSolve <- function(m_cache, ...) {
  
  #gets inverse value stored in memory
  inv<-m_cache$get_inv()
  
  #returns cached data
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  #computes the inverse if the value stored in memory is NULL
  data<-m_cache$getm()
  inv<-solve(data)
  
  #stores the value in memory
  m_cache$set_inv(inv)
  
  inv 
}
