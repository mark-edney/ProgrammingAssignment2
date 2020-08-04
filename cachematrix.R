## The first function will creates a matrix that will be stored in cache. 
##The second function will than determine if the inverse has already been found
##and stored in cache. If it has not been stored, it will find the the inverse
##with the solve function

## The makeCacheMatrix will create the matrix that will be stored in cache

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(solve) m <<- solve
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}

x<-matrix(c(2,2,1,-3,0,4,1,-1,5),nrow=3,ncol=3)

