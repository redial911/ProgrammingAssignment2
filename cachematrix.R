## A set of functions that will take a matrix as input, check to see if it is square,
## and then compute its inverse (if possible).  The inverse is cached so that it
## won't have to be re-computed in case it is needed again.

##                      **********

## 'makeCacheMatrix()' is a 'constructor' function that takes a matrix as its 
## argument, creates 4 'closure' functions that will initialize, store, 
## retrieve, and compute the inverse of this matirix (if it has one), and
## returns a named list of the 4 functions.

makeCacheMatrix <- function(x = matrix()) {
        ## create a variable in which to store the inverse of the matrix.
        inverse.x <- NULL
             
        ## 'set()' is a closure function that encloses the environment of
        ## 'makeCacheMatrix', its parent function. Whenever it is called, its job
        ## is to initialize to NULL the global variable that will hold the inverse
        ## & to store the original matrix in cache memory.
        set <- function(y) {
                x <<- y       ## the 'double arrow" assign creates 'x' if it doesn't exit.
                inverse.x <<- NULL
        }
        
        ## 'get()' is a closure function to retrieve the original matrix from cache
        get <- function() {
                x
        }
        
        ## 'setinv()' - a closure function to compute the inverse of the original
        ## matrix & store it in cache.
        setinv <- function(x) {
                inverse.x <<- solve(x)
        }
        
        ## 'getinv()' - a closure function to retrieve the inverse matrix from cache.
        getinv <- function() {
                inverse.x
        }
        
        ## return the above functions in a named list.
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)     
}


## 'cacheSolve()' is a function that calls the closure functions created by
## makeCacheMatrix.  It retrieves the original matrix stored by makeCacheMatrix using
## x$get() and tests to see if it is a square matrix.  If square, it either computes
## the inverse or retrieves it from cache, and then returns it.

cacheSolve <- function(x, ...) {
        
        ## Retrieve the original matrix stored in cache by makeCacheMatrix.
        tm <- x$get()
        ## Is it squeare? If not, stop with error message.
        if(nrow(tm) != ncol(tm)) {
                stop("Martix isn't square!")
        }
        ## pull the inverse matrix value from cache. If its value is not NULL, it's
        ## already been computed. So, return it and exit the function.  
        inverse.x <- x$getinv()
        if(!is.null(inverse.x)) {
                message("getting cached data")
                return(inverse.x)
        }
        ## The inverse matrix value in cache is NULL, so compute it.
        inverse.x <- solve(tm)
        ## now save the inverse matrix in cache.
        x$setinv(tm)
        ## and finally, return it.
        inverse.x
        
        
}
