## A function that calculates the inverse of a matrix and a function that caches the calculated inverse

##creating function that creates a matrix and can cache its own inverse; requires matrix as only argument

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL                               #creating an empty object and assign it variable m 

        set <- function(y) {                    #function with argument y
                x <<- y                         #set the cached value y, assign it to x, in the parent environment
                m <<- NULL                      #set the cached m to an empty object in the parent environment
        }

        get <- function() x                     #function that returns the cached matrix
        setinverse <- function(inverse) m <<- inverse      #function that requires the inverse amount as the argument; sets the cached inverse
        getinverse <- function() m              #function that returns the cached inverse value of the matrix

        list(set = set, get = get,             #create a list of the four functions now created
             setinverse = setinverse,
             getinverse = getinverse)
}


## function that either computes the inverse of the matrix created by makeCacheMatrix or retrieves the cache if it already has been calculated

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {                      #if function that looks to see if m is empty; if m is not empty then runs message and returns m; ends the function
                message("getting cached data")
                return(m)
        }

        data <- x$get()                        #if m is empty then finds the matrix from makeCacheMatrix and assign it to data
        m <- solve(data, ...)                  #calcuates the inverse by using the solve function and assigns it to m
        x$setinverse(m)                        #assigns the inverse to the cacheSolve function
        m                                      #returns the calculated inverse
}
