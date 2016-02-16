## Calculate the inverse of a matrix using the properties of Lexical Scoping in R
## Carlos Gutierrez Sanchez del Rio 2016

## Calculates the inverse of a matrix and stores it in cache. When the function is called again, it checks if the calculation is 
## already stored in the cache and retrieves it (calculates it otherwise). The function also checks if the matrix has an inverse.
## Usage example: 
## >test<- matrix(c(2,5,1,3),nrow=2)
## >temp<-makeCacheMatrix(test)
## >cacheSolve(temp)


## makeCacheMatrix: creates a special "matrix" object that can cache its inverse

makeCacheMatrix<- function (x=matrix()){
    invx<-NULL
    set<-function(y){
        x<<-y
        invx<<-NULL
        ## sets the matrix (the inverse is still empty)
    }
    get <-function(){
        x
        ## returns the matrix
    }
    setInverse<- function(inv){
        invx<<-inv
        ## stores the inverse of the matrix
    }
    getInverse<- function(){
        invx
        ## returns the inverse of the matrix stored in cache
    }
    list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## cacheSolve: computes the inverse of the special "matrix" returned by makeCacheMatrix
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.


cacheSolve <- function (x,...){
  invx<-x$getInverse()
  if(!is.null(invx)){
      message("getting inverse matrix from cache...")
    ## checks if the inverse matrix of x is stored in cache
  } 
  else {
    ## inverse matrix is not in cache and needs to be calculated
    data<-x$get()
    ## a matrix has an inverse if it is square (nrow=ncol) and its determinant is non-zero
    ## a tolerance for checking if the determinant is 0 has been included because of the way the determinant is calculated (depending on the system and local configuration)
    ## R returns -3.330669e-16 instead of 0 for det(matrix(c(2,6,1,3),nrow=2)):
    
    if ((abs(det(data))>=1e-10)&&(nrow(data)==ncol(data))) {
      invx<-solve(data,...)
    } else {
      print("Matrix doesn't have an inverse. Make sure it is square and determinant!=0")
      return()
    }
    x$setInverse(invx)
  }
  invx
  ## returns  a matrix that is the inverse of 'x' if the matrix has an inverse
}
