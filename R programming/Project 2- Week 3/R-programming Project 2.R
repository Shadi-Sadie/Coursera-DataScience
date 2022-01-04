
# Writing a pair of functions that cache the inverse of a matrix.For this assignment, we assume that the matrix supplied is always invertible.

# first function  makeCacheMatrix

makeCacheMatrix<- function(x = matrix()) {
        m <- NULL
        # set the matrix
        set <- function(y) {
            x <<- y
            m <<- NULL
        }
        #get the matrix
        get <- function() x
        # seting the inverse matrix
        setinv <- function(inv) m <<- solve
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
    }

# second function cacheSolve


cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
        if(!is.null(m)) {
            message("getting cached data")
            return(m)
        }
        data <- x$get()
        # calculate the inverse of matrix
        m <- solve(data, ...)
        x$setinv(m)
        # return the matrix
        m
}
    

# Testing it 

A <- matrix( c(5, 1, 0,
               3,-1, 2,
               4, 0,-1), nrow=3, byrow=TRUE)
myMat<-makeCacheMatrix(A)
cacheSolve(myMat)




