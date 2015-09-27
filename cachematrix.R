## makeCacheMatrix creates a matrix 'x' and functions that
## set the value of 'x', returns the matrix 'x', sets the inverse
## of 'x' and gets the inverse of 'x'

makeCacheMatrix <- function(x = matrix()) {
              ## i holds the inverse of 'x', initialized to NULL            
              i<-NULL 
              set<-function(y){
                ## x is initialized to the value specified by the parameter y
                x<<-y  
                ## i is initialized to NULL for every "new" matrix
                i<<-NULL  
              }
              ## returns the matrix 'x'
              get<-function() x     
              ## i is initialized to the inverse of the matrix passed as inv
              setinverse<-function(inv) i<<-inv  
              ## returns the inverse of 'x'
              getinverse<-function() i  
              list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
         
}


## cacheSolve retrieves the inverse of a matrix through the getinverse() function.
## If the inverse of the matrix is not calculated, it calculates it
## via the solve(x) function, otherwise the cached inverse is retrieved

cacheSolve <- function(x, ...) {
        ## gets the inverse of matrix 'x'
        i<-x$getinverse()
        ## checks if inverse is pre calculated or not
        if(!is.null(i)){
          message("getting cached inverse")
          return(i)
        }
        ## if inverse is not cached, get the data of 'x'
        data<-x$get()
        ## calculate the inverse through built in function solve(X)
        i<-solve(data,...)
        ## set the inverse in the cached matrix
        x$setinverse(i)
        ## return the inverse
        i
}
