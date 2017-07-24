## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

#This function is made to cache its invers in a square matrix

# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inv<-NULL ## sets inv to NULL in order to provide a default value if cacheSolve has not been used
        set <-function(y){
                x<<-y ##set the value
                inv<<-NULL ##clear the cache
        }
        ##here is defined the function able to get the value of the matrix
        get<-function()x
        
        ##define the function to set the inverse. This is only used by getinverse()
        ##when is no chached inverse
        
        setinverse<-function(inverse) inv<-inverse
        
        ##define function to get the its inverse
        getinverse<-function () inv
        
        ##return a list with the above established functions
        list(set=set,
             get=get,
             setinverse=setinverse,
             getinverse=getinverse)
             
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), 
##then the cachesolve should retrieve the inverse from the cache

cacheSolve<-function(x, ...){
        ## Return a matrix that is the inverse of 'x'
        
        inv<-x$getinverse()
        
        ## if the cach is not empty, a massage is returned
        
        if(!is.null(inv)){
                message("getting cached matrix")
                return(inv)
        }
        data<-x$get()
        inv<-solve(data, ...)
        x$setinverse(inv)
        inv
        
}



