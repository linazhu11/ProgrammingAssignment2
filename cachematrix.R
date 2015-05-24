## The functions below, "makeCacheMatrix" and "cacheSolve," calculate the inverse of
## square, numeric matrix and store the results. These matrix objects can then be
## retrieved later without repeating calculations. 

## "makeCacheMatrix" creates a list of functions for a numeric matrix object,
## allowing the user to define and cache both the matrix and its inverse

makeCacheMatrix <- function(x = numeric()) {
    i <- NULL

    setmatrix <- function(y) {
        x <<- y
        i <<- NULL
    }

    getmatrix <- function() x

    setinverse <- function(inv) i <<- inv

    getinverse <- function() i

    list(setmatrix = setmatrix, getmatrix = getmatrix, setinverse = setinverse, getinverse = getinverse)
}


## "cacheSolve" returns the inverse of the matrix defined in "makeCacheMatrix" above
## It first checks to see if the inverse already exists. If so, it returns the cached inverse. 
## Otherwise, it proceeds to calculate the matrix inverse.

cacheSolve <- function(x, ...) {
       i <- x$getinverse()
       
       if (!is.null(i)) {
           message("getting cached data")
           return(i) #returns existing inverse and exits the function
       }
       
       matrix <- x$getmatrix()
       i <- solve(matrix) #calculates inverse
       x$setinverse(i)
       i
}
