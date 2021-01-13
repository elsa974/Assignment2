
## This function creates a special matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
       s<-NULL
       set<-function(y){
              x<<- y
              s<<- NULL
       }
       get<-function(){x}
       setInverse<- function(inverse) {s<<- inverse}
       getInverse<- function() {s}
       list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


##This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
##If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        s<- x$getInverse()
        if(!is.null(s)){
               message("getting cached data")
               return(s)
        }
        data<-x$get()
        s<-solve(data,...)
        x$setInverse(s)
        s
}
