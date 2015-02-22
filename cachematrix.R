##Please note that to test this function, you have to use these four lines of codes below:

#source("cachematrix.R") #Extract the function from cachematrix.R file.
#xx <- makeCacheMatrix() # set function makeCacheMatrix() to variable xx
#xx$set(matrix(1:4, nrow =2, ncol = 2)) #set var xx to be a 2 by 2 square matrix with set sub function in makeCacheMatrix
#cacheSolve(xx) #Test cacheSolve() function to see whether it returns the inverse matrix stored in cache.


## This function makeCacheMatrix caches the matrix and inverse of matrix in a list with
#set , get ,setinverse & getinverse subfunctions
makeCacheMatrix <- function(x = matrix()) {
    
        m <- NULL
        set<- function(y) { #creates a sub-function called "set", which takes an argument "y".
           x<<-y
           m <<- NULL
           
       } 
       get <- function() x  #creates a sub-function called "get", which reports the value of x
       setinverse <- function(solve) m <<- solve ## setting the inverse of a square matrix
       getinverse  <- function() m #creates a sub-function called "getinverse", which reports the value of "m".
       
       list(set = set, get = get, #create the list for you to use the 4 subfunctions when you assign to a variable,
            setinverse = setinverse,
            getinverse = getinverse)

}

## This function cacheSolve Returns a matrix that is the inverse of 'x'

    cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
            message("getting cached data")
            return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
    }


