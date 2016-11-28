## This script returns chached inverted matrixes if it has allready been calculated, 
## else it calculates and caches the inverted matrix

## This function takes an argument that has to be a square and invertible matrix and
## returns a list of functions to:
##      1 set the matrix
##      2 get the matrix
##      3 set the inverse
##      4 get the inverse

makeCacheMatrix <- function(x = matrix()) {
        
        mat <- NULL
        set <- function(y) {
                x <<- y
                mat <<- NULL
        }
        get <- function() x
        setinvert <- function(invert) mat <<- invert
        getinvert <- function() mat
        
        list(set=set, get=get, setinvert=setinvert, getinvert=getinvert)

}

## This function looks if we have cached the inverted matrix previously. If it has been catched the inverted
## matrix will be returned else the function calculates the inverted matrix and caches it

cacheSolve <- function(x, ...) {
        
        mat = x$getinvert()
                if(!is.null(mat)){
                        message("getting cached data")
                        return(mat)
                }
        matrix <- x$get()
        mat = solve(matrix, ...)
        
        x$setinvert(mat)
        
        return(mat)
        
}

## just some testing of the functions...
set.seed(1110201)
r = rnorm(9)
mat1 = matrix(r, nrow=3, ncol=3)
mat2 = matrix(r, nrow=3, ncol=3)
mat1
mat2
test <- makeCacheMatrix(mat1)
test2 <- makeCacheMatrix(mat2)
test
test2
cacheSolve(test)
cacheSolve(test2)

