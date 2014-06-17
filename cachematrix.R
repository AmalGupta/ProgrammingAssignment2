## Programming Assignment 2 to cache the inverse of a matrix.
## The programming assignment works with two functions 
## * makeCacheMatrix < - which makes a special data structure to cache the inverse of a matrix and return it. It can
##                       be viewed as method which helps creates the environment variable. 
## Usage :- the following two lines let you know how to invoke the methods. 
##              a<-makeCacheMatrix()   # create a cache matrix structure with default value
##              a$set(matrix(1:4,2,2),2,2)     # Assign the matrix     
##
##  * cacheSolve() which validates whether the matrix entered is in the cache. If so, the output is displayed 
## from the cache, otherwise, the output is calculated.

## The function returns a data structure from the entered matrix. It has the method set, get, setmatrix, getmatrix etc.
makeCacheMatrix <- function(x = matrix()) {
        m<-NULL
        set<-function(y){
                x<<-y
                m<<-NULL
        }
        
        get<-function() x
        setmatrix<-function(solve) m<<- solve
        getmatrix<-function() m
        list(set=set, get=get, setmatrix=setmatrix, getmatrix=getmatrix)
}


## This function solves for the inverse if the inverse is not already in the cache. Once the inverse is calculated,
## it stores the data in the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m<-x$getmatrix()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        
        matrix<-x$get()
        m<-solve(matrix, ...)
        x$setmatrix(m)
        m
}
