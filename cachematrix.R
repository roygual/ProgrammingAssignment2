## Put comments here that give an overall description of what your
## functions do


## This function creates an object which contains a matrix and returns a list of
## functions, this list of functions will allow us to manipulate the 
## matrix within, in the form of new.object$get() or new.object$set(a.matrix).

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        
        set <- function(y) 
        {
                x <<- y
                m <<- NULL
        }
        
        get <- function() 
        {
                x
        }
        
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        
        list(set = set, get = get, setinverse=setinverse, getinverse = getinverse)
}



## This function receives the object created with makeCacheMatrix, process the
## data of the matrix within it with solve(), make use of the functions listed 
## above in the first commentary and stores the results in a cache on the
## local variable "m".... returns this "m" variable

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        m <- x$getinverse()
        
        if(!is.null(m)) 
        {
                message("getting cached data")
                return(m)
        }
        
        data <- x$get()
        
        m <- solve(data, ...)
        
        x$setinverse(m)
        
        m
}

