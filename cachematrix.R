## makeCacheMatrix(): It creates a speical vector , which is really a list containing  a function
## to set and get the value of invertible matrix elements & set and get the value of its inverse 

## cacheSolve(): It returns the inverse matrix of the matrix obtained from above function. It 
## first check whether its inverse is present in the cache (i.e. matrix is previously computed for its 
## inverse),or not. If its inverse is present, the function returns its inverse from cache or 
## otherwise it calculates the inverse by using solve() function.

## This function inputs a matrix, it get & set the matrix's elements value and then get and
##set its inverse

makeCacheMatrix <- function(x = matrix()) {
  i<- NULL
  set<- function(y){
    x<<- y 
    i<<-NULL
    
  }
  get <- function() x
  setinv <- function(inverse) i<- inverse
  getinv<- function() i
  list(set=set, get=get,setinv=setinv, getinv=getinv)

}


## This  function returns the inverse matrix from cache if matrix is already computed for
## its inverse, otherwise it computes the inverse of the matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i<-x$getinv()
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  data<- x$get()
  i<- solve(data,...)
  x$setinv(i)
  i
  
}
