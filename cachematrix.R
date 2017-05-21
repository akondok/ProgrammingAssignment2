##  caching the inverse of a matrix in order to reduce the computation costs involed in computing it repeatedly

## The following function receives a matrix and creates a special list of functions that set the value of the matrix,
## gets the value of the matrix, sets the value of the inverse of the matrix and gets the value of the inverse of the matrix

makeCacheMatrix<-function(x=matrix())
{
  i<-NULL
  set <-function(y)
  {
    x<<-y
    i<<-NULL
  }
  get<-function() x
  
  setinverse<-function(inv)
  {
    i<<-inv
  }
  
  getinverse<-function() i
  
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}



## The following function receives a list created by makeCacheMatrix and computes and checks if the inverse is already computed
## if the inverse has been computed then it fetches it from the cache and returns it. otherwise it computes it and caches it using makeCacheMatrix
## before returning it.
cacheSolve <- function(x, ...) 
  {
    i<- x$getinverse()
    
    if(!is.null(i)) 
      {
        return(i)
      }
    
    theMatrix <- x$get()
    
    i<-solve(theMatrix)
  
    x$setinverse(i)
  
    i  ## Return a matrix that is the inverse of 'x'
}
       
