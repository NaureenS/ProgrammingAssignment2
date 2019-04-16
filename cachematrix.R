
##R Programming Week 3 assignment - NaureenS

##Caching the inverse of a matrix 

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL     ##Initialize the inverse matrix as NULL
        
        set <- function(y) {    ##Set the value of the matrix
          x <<- y
          i <<- NULL      
        }
        get <- function() x     ##Define the get function to return the matrix
        
        ##Assigns the value of the inverse in the parent environment
        setinverse <- function(inverse) i <<- inverse 
        ##Gets the value of the inverse
        getinverse <- function() i  
        
        #To use the $ operator
        list (set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Calculate the inverse of the special "Matrix" created with the above function 

cacheSolve <- function(x, ...) {
       
   ## Return a matrix that is the inverse of 'x'
       
         i <- x$getinverse()
        if(!is.null(i)) {
          message("getting cached data")
          return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}

