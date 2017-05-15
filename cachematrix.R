## The steps to run the methods are
## 1. m <- makeCacheMatrix(matrix(c(1,2,3,4), nrow=2, ncol=2))
## 2. m$get()  #returns the matrix of 1, 2, 3, 4
## 3. cacheSolve(m)  #this is the first time it calculates the inversed matrix so it won't be cached
## 4. cacheSolve(m)  #the step above already put the inversed matrix in cache so it gets the cached result and you'll find the message of "getting cached inverse matrix"


## Takes a matrix and registers methods of get, set, setinverse and getinverse for the input matrix
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL

        set <- function(y) {
                x <<- y
                m <<- NULL
        }

        get <- function() x

        setinverse <- function(i) m <<- i
        
        getinverse <- function() m
        
        list(set = set,
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Return a matrix that is the inverse of 'x'
## if it's the first time to calculate, then calculate and save the result in cache
## if it's already cached, return the cached result
cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if (!is.null(m)) {
                message("getting cached inverse matrix")
                return(m)
        }

        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
