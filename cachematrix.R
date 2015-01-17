## Description of steps employed and arguments in makeCacheMatrix()
## 1. mtrx is initialized and it saves the inverse matrix later, this is the one that is cached.
## 2. x is the raw invertible square matrix
## functions are:
## 3. setMatrix      set the value of a matrix
## 4. getMatrix      get the value of a matrix
## 5. setImatrix     get the cahced value (inverse of the matrix)
## 6. getInverse     get the cahced value (inverse of the matrix)

makeCacheMatrix <- function(x = matrix()) {
 mtrx <- NULL
    getMatrix <- function() x						## returns the stored matrix
    setMatrix <- function(m) {						## store a matrix
        x <<- m                
        mtrx <<- NULL
    }
    setImatrix <- function(Imatrix) mtrx <<- Imatrix 					## cache the given argument 
    getImatrix <- function() mtrx 					## get the cached value

    # Creating a List, to return functions as an R object
    list(setMatrix = setMatrix, getMatrix = getMatrix, setImatrix=setImatrix, getImatrix=getImatrix)
}


## Description of steps employed and arguments in cacheSolve()
## 1. The following function calculates the inverse of a "special" matrix created with makeCacheMatrix.
## 2. It first checks if the inverse matrix has been found; 
## 3. if yes, returns the result and quits. If not, then inverse of x is calculated, cached, and returned;
## 4. x param must be cached, i.e. a list returned from calling makeCacheMatrix(x).

cacheSolve <- function(x, ...) {
 
 Imtrx <- x$getImatrix()							## Returning inverse of x matrix.
    if(!is.null(Imtrx)){
        message("Cached data found")
        message("Getting result... Done.")
        return(Imtrx)
    }
    else {
        message("Cached data not found")
        data <- x$getMatrix()						## obtaining matrix from 'x'
        Imtrx <- solve(data) 						## finding inverse matrix
        message("Calculating inverse matrix...")
        x$setImatrix(Imtrx) 												
        message("Done.")
        return(Imtrx)
    }
}
