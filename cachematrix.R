## The first function will creates a matrix that will be stored in cache. 
##The second function will than determine if the inverse has already been found
##and stored in cache. If it has not been stored, it will find the the inverse
##with the solve function

## The makeCacheMatrix will create the matrix that will be stored in cache

makeCacheMatrix <- function(x = matrix()) {
##Initialize the matrix inverse m for the matrix x
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
##set of get/set functions        
        get <- function() x
        setinv <- function(solve) m <<- solve
        getinv <- function() m
        
##function returns a list of functions, allows functions to be refered by name        
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)

}


## Function that looks for a value of the inverse in the cache and finds the 
##inverse if it isnt store. Relies on the functions passed by markCachematrix

cacheSolve <- function(x, ...) {
##uses getinv function to find m
        m <- x$getinv()
        
##checks if the value is stored(ie not null)
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
#gets the matrix and finds the inverse
        data <- x$get()
        m <- solve(data, ...)
#stores the inverse in cache 
        x$setinv(m)
        m
}
