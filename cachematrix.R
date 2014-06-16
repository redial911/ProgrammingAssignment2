## A set of functions to take a matrix as input, check to see if it is square,
## and then compute its inverse if possible.  The inverse is cached so that it
## won't have to be re-computed in case it is needed again.


## 'makeCacheMatrix' is a 'constructor' function takes a matrix as its 
## argument, creates & returns a list of 4 functions that initialize, store, 
## retrieve, and compute the inverse of the matirix (if it has one).

makeCacheMatrix <- function(x = matrix()) {
        ## create a global variable in which to store the inverse of the matrix.
        inverse.x <- NULL
             
        ## 'set' is a 'closure' function that encloses the environment of
        ## 'makeCacheMatrix', its parent function. Whenever it is called,
        ## its job is to initialize the global variable that will hold the inverse to NULL
        ## & to store the original matrix in cache memory.
        set <- function(y) {
                x <<- y
                inverse.x <<- NULL
        }
        
        ## 'get' is a closure function to retrieve the original matrix from cache
        get <- function() {
                x
        }
        
        ## 'setinv' - a closure function to compute the inverse of the original
        ## matrix & store it in cache.
        setinv <- function(x) {
                inverse.x <<- solve(x)
        }
        
        ## 'getinv' - a closure function to retrieve the inverse matrix from cache.
        getinv <- function() {
                inverse.x
        }
        
        ## return the above functions as a named list.
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)     
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        tm <- x$get()
        if(nrow(tm) != ncol(tm)) {
                stop("Martix isn't square!")
        }
        inverse.x <- x$getinv()
        if(!is.null(inverse.x)) {
                message("getting cached data")
                return(inverse.x)
        }
        data <- x$get()
        inverse.x <- solve(data)
        x$setinv(data)
        inverse.x
        
        
}
