## This file is submission of Assignment 2, where we exploit lexical scoping, 
## and try to cache the result of matrix inverse, to avoid repeating the 
## operation.  


## Description: Function responsible to cache a matrix and it's inverse and 
##              provide API to set or get a matrix and set or
##              get inverse of a matrix. 
## Function   : makeCacheMatrix
## Input      : Matrix
## Output     : list object of functions operating on Matrix, to set/get matrix 
##              and setInverse/getInverse inverse of matrix. 

makeCacheMatrix <- function(x = matrix()) {
    inverseMatrix <- NULL
    set <- function(y) {
        x <<- y
        inverseMatrix <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inverseMatrix <<- inverse
    getInverse <- function() inverseMatrix
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Description : Function to return inverse of a matrix, either from cache or
##               by calculating it. 
## Function    : cacheSolve
## Input       : list object of function containing set, get, setInverse and 
##               getInverse for a matrix.
## Output      : Inverse Matrix

cacheSolve <- function(x, ...) {
    inverseMatrix <- x$getInverse()
    if(!is.null(inverseMatrix)) {
        print("we getting inverse matrix from cache")
        inverseMatrix
    }
    matrix <- x$get()
    inverseMatrix <- solve(matrix)
    x$setInverse(inverseMatrix)
    inverseMatrix
}
