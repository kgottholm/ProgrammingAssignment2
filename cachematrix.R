# The first function, makeCacheMatrix creates a special "matrix", 
# which is really a list containing a function to
# set the value of the matrix
# get the value of the matrix
# set the value of the inverse
# get the value of the inverse
## perform calculations on inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
      
        
        ## define a cache 
        cacheit <- NULL
        set <- function(y) {
                x <<- y ## assign the input matrix y to the variable x 
                cacheit <<- NULL ## clear cache
        }
        get <- function() x ## get the matrix
        buildinverse <- function(inverse) cacheit <<- inverse ## put in cache
        ## to the inverse of the matrix x
        getinverse <- function() cacheit ## put in cache
        list(set = set, get = get,
             buildinverse = buildinverse,
             getinverse = getinverse)
}
}


cacheSolve <- function(x, ...) {
        ## build a revere matrix
        
        cacheit <- x$getinverse()
        if(!is.null(cacheit)) {
                message("getting cached data")
                return(cacheit)
        }
        data <- x$get()
        cacheit <- solve(data, ...)
        x$buildinverse(cacheit)
        cacheit
}
