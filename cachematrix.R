## "makeCacheMatrix" function creates a special "matrix" which is a list of functions
## "cacheSolve" function receives the special matrix created by the "makeCacheMatrix" function and returns the corresponding inverse matrix

## "makeCacheMatrix" function receives a matrix then created a special "matrix" which is a list of functions for
##      1. setting the matrix value 
##      2. getting the matrix value
##      3. setting the inverse matrix
##      4. getting the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
    inverseMatrix <- NULL
    set <- function(newMatrix) {
        x <<- newMatrix
        inverseMatrix <<- NULL
    }
    get <- function() x
    
    setInverseMatrix <- function(cachedInverseMatrix) inverseMatrix <<- cachedInverseMatrix
    getInverseMatrix <- function() inverseMatrix
    
    list(set = set, get = get, setInverseMatrix = setInverseMatrix, getInverseMatrix = getInverseMatrix)
}


## "cacheSolve" function receives a special "matrix" created by the "makeCacheMatrix" function.
## It queries the corresponding inverse matrix. 
##      If the inverse matrix is not null then "cacheSolve" returns it.
##      Otherwise, "cacheSolve" computes the inverse matrix, saves it into the special "matrix" then returns the computed inverse matrix
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    inverseMatrix <- x$getInverseMatrix()
    if (!is.null(inverseMatrix)) {
        message("getting cached inverse matrix")
        return(inverseMatrix)
    }
    data <- x$get()
    inverseMatrix <- solve(data)
    x$setInverseMatrix(inverseMatrix)
    inverseMatrix
}
