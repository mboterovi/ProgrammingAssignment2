## This is the assignment 2 for week 3 that consist in 2 functions:

## 1. makeCacheMatrix function: This function stores the inverse of a matrix 
##    using the "solve" function, the result is stored in cache for future 
##    references. It use a different environment (using "<<-" symbol).

makeCacheMatrix <- function(x = matrix()) {
        m<-NULL
        set<-function(y){
                x<<-y
                m<<-NULL
        }
        get<-function() {
                x
        }
        setinverse<-function(solve) {
                m<<- solve
        }
        getinverse<-function() {
                m
        }
        list(set=set, get=get,
             setinverse=setinverse,
             getinverse=getinverse)
}


## 2. cacheSolve function: This function also calculates the inverse of a 
##    matrix using "solve" if there is not a previous inverse calculation  
##    of the same matrix stored in cache. Although, if there is a previous 
##    inverse matrix calculation stored in cache of the same matrix, it 
##    returns a message like "getting inverse matrix cached data" and the 
##    inverse matrix that was stored in cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m<-x$getinverse()
        if(!is.null(m)){
                message("getting inverse matrix cached data")
                return(m)
        }
        matrix<-x$get()
        m<-solve(matrix, ...)
        x$setinverse(m)
        m
}
