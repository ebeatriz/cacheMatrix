# cacheMatrix
ProgrammingAssignment2 for R Programming on Coursera
##  makeCacheMatrix creates a list containing several functions to:
##  set the value of a matrix
##  get the value of a matrix
##  set the inverse of a matrix
##  get the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) m <<- inverse
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

##  cacheSolve calculates the inverse of the matrix created with the above
##  function. cacheSolve can do it either (1) returning the inverted matrix stored
##  in getinv or (2) calculating it with a solve function and passing the
##  arguments to setinv.

cacheSolve <- function(x, ...) {
        ##  Firstly, if there is an inverse matrix already stored, cacheSolve
        ##  recovers it
                m <- x$getinv()
                if(!is.null(m)) {
                        message("Getting data from cache")
                        return(m)
                }
        ## The next is a sort of else{} and return a matrix that is the inverse 
        ## of 'x'
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}
