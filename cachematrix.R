## Functions which help cache and retrieve matrix inverses
## If matrix inverse for a specific matrix is already calculated,
## it'll return from the cache. Else, it will calculate and
## fill the cache with the value

## This function defines what each of the member functions
## are supposed to do

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y){
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set=set, get=get, setinverse=setinverse, 
         getinverse=getinverse)
}


## If inverse is already calculated, return the cached value
## Else calculate and then save the value in the cache

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if (! is.null(m)) {
        message("Getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
