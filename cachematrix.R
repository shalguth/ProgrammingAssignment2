## Functions below are intended to decrease the time it takes to 
## solve the inverse of a matrix for large vectors

## Create a matrix object that can cache the inverse of the matrix given
makeCacheMatrix <- function(x = matrix()) {
    m<-NULL
    set<-function(y) {
        x<<-y
        m<<-NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set=set, get=get, setinverse=setinverse,
         getinverse=getinverse)
}


## Computes inverse of matrix from makeCacheMatrix function. If inverse has 
## previously been calculated then it will be retrieved from the cache
cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m ##Where m is the inverse of matrix x
}
