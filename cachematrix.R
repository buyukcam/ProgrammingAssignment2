## (1) prepare a square matrix 
## (2) use makeCacheMatrix to construct from that matrix a matrix with special methods
## (3) use cacheSolve

##              cacheSolve can find the inverse of the special matrix passed-in
##              The "Special" description means that the matrix can store its own inverse
##              that can be immediately returned (without re-calculation)


## makeCacheMATRIX:  Returns a list of functions to:
##    set the value of the matrix passed-in
##    get the value of the matrix passed-in
##    set the value of the inverse matrix to that matrix passed-in
##    get the value of the inverse matrix to that matrix passed-in

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL                                  ############################
        set <- function(y) {                       #set the value of the matrix
                x <<- y
                m <<- NULL
        }
        get <- function() x                        #get the value of the matrix
        setsolve <- function() m <<- solve         #set the value of the inverse
        getsolve <- function() m                   #get the value of the inverse
        list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}


## cacheSolve:  Returns a matrix that is the inverse of 'x'
##    If cacheSolve has previously calculated the inverse, 
##    it is merely retrieved from a cached value in the special matrix, 
##    instead of performing 'solve' over again.

cacheSolve <- function(x, ...) {
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve()
        m
}
