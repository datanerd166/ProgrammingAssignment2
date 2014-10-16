## cachematrix.R contains the following functions
##      makeCacheMatrix: creates a special matrix that caches its inverse matrix.  
##      cacheSolve: returns an inverse of the special matrix created by makecacheMatrix
##
## Testing:     1) Create a numeric matrix:     m <- matrix(rnorm(9), 3,3)
##              2) Call makeCacheMatrix():      cm <- makeCacheMatrix(m)
##              3) Call cacheSolve():           mi <- cacheSolve(cm)
##              4) verify M * M_Inverse = I:    m %*% mi

## makeCacheMatrix: Creates a list of supporting functions to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse matrix
## 4. get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        ## Initialize the inverse of the matrix to null
        inv_x <- NULL
        
        ## Set the value of the matrix
        set <- function(y) {
                x <<- y
                inv_x <<- NULL
        }
        
        ## Get the value of the cached matrix
        get <- function() x
        
        ## set the value of the inverse matrix
        setInverse <- function(inv) inv_x <<- inv
        
        ## get the value of the inverse matrix
        getInverse <- function() inv_x
        
        ## Create a list of functions and return the list
        list(set = set, get = get, 
             setInverse = setInverse, 
             getInverse = getInverse)
}

## cacheSolve:  Verifys if the inverse of the matrix created by makeCachedmatrix already exists 
##              in cache. If it is, returns the cached inverse matrix; otherwise, creates an 
##              inverse of the matrix x first, saves it in cache, then returns it.

cacheSolve <- function(x, ...) {
        ## Get the value of the inverse of the matrix from cache
        inv_x <- x$getInverse()
        
        ## Verify if the inverse of the matrix is already created, then return it
        if (!is.null(inv_x)) {
                message("getting cached inverse matrix of x")
                return(inv_x)
        }
        
        ## The inverse matrix does not exist:
        ## get the value of matrix, create its inverse and save the inverse matrix
        data <- x$get()
        inv_x <- solve(data, ...)
        x$setInverse(inv_x)
        
        ## Return the inverse of matrix 'x'
        inv_x
}
