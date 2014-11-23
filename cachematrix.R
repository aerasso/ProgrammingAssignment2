## Assignment 2


## This first call to makeCacheMatrix() creates a matrix.
## The next call, this time to cacheSolve, calculates the inverse of matrix 
## (if square and invertible), stores and returns the 'inverse' with the 
## first cacheSolve (matrix) access of the matrix; if not available it then
## passes a previously stored inverse matrix with the second cacheSolve(matrix)
## access of the matrix


## makeCacheMatrix creates a matrix and defines functions to access it or its 
## inverse

makeCacheMatrix <- function(x = matrix()) {

   m <- NULL    ## m will be the 'inverse' and it is reset to NULL every time 
                ## makeCacheMatrix is called
   
   ## The next four functions are defined but not run when makeCacheMatrix
   ## is called. They will be used by cacheSolve to get values for x or for
   ## m (the inverse) and for setting the inverse using function solve().
     
   set <- function(y) {
       x <<- y
       m <<- NULL
   } 
   get <- function() x
   
   setinverse <- function(solve) m <<-solve ## This is called by cacheSolve 
                                            ## during the first cacheSolve
                                            ## access and will store the 
                                            ## value using superassignment
   
   getinverse <- function() m               ## This will return the cached 
                                            ## value to cacheSolve on
                                            ## subsequent accesses
   
   ## The list returned by makeCacheMatrix
   list(set = set, get = get, setinverse=setinverse, getinverse=getinverse)
      
}

## cacheSolve returns the inverse of a matrix created by makeCacheMatrix

cacheSolve <- function(x, ...) {
            
        m <- x$getinverse()

        if(!is.null(m)) {     ## If inverse was already cached (not NULL)
        
            message("getting cached data")
            return(m)
        
        }
        
        ## If inverse was NOT already cached (NULL)
        data <- x$get()
        
        m <- solve(data, ...)
        
        x$setinverse(m)
        
        m 
    
}
