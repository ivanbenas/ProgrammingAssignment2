## The function is an utility useful to save computing time while setting 
## the result of a hard task

## this function saves the value of the inverse matrix to not compute it
## more than once
makeCacheMatrix <- function(x = matrix()) {

        m <- NULL
        set <- function(y) {
                x<<-y
                m<<-NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        message("setting  data")
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The function returns the inverse of a matrix.
# This function assumes that the matrix is always invertible.
# If the same matrix was calculated previously, the program returns
# the stored value.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        
        if(!is.null(m)) {
                
                message("getting cached data")
                return(m)
        }
        matrix <- x$get()
        m <- solve(matrix, ...)
        x$setinverse(m)
        m
}
