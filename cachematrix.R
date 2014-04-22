## Summary description of functions:
## Copied and modified from the assignment website
## makeCacheMatrix creates a special matrix object that can cache its inverse.
## cacheSolve computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and
## the matrix has not changed), then the cacheSolve would retrieve the inverse
## from the cache.

## makeCacheMatrix: 
## Creates a special matrix that contain the following 4 functions to
## 1. Set value of the source matrix (setMatrix)
## 2. Get value of the source matrix (getMatrix)
## 3. Set value of the inverse of the source matrix (setInverseMatrix)
## 4. Get value of the inverse of the source matrix (getInverseMatrix)

makeCacheMatrix <- function(x = matrix()) {
    inversedMatrix <- NULL
    
    setMatrix <- function (mat) {
        srcMatrix <<- mat
        inversedMatrix <<- NULL
    }
    
    getMatrix <- function () {
        x # which is the original matrix
    }
    
    setInverseMatrix <- function (invMat) {
        inversedMatrix <<- invMat
    }
    
    getInverseMatrix <- function () {
        inversedMatrix
    }
    
    list (setMatrix = setMatrix,
          getMatrix = getMatrix,
          setInverseMatrix = setInverseMatrix,
          getInverseMatrix = getInverseMatrix)
}


## cacheSolve:
## calculates the inverse of the special matrix created in makeCacheMatrix.
## First checks if the inverse has been calculated. If so, gets the inverse from
## the cache and skip the computation. Otherwise, it calculates the inverse of
## the data and sets the value of the inverse matrix in the cache via the
## setInverseMatrix function.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    invMatrix <- x$getInverseMatrix()
    
    ## check against cache, if the inverse is already there, skip computation
    if (!is.null(invMatrix)) {
        return (invMatrix)
    } else {
        ## get the source matrix, calculate inverse, store and return
        srcMatrix <- x$getMatrix()
        calcInverseMatrix <- solve(srcMatrix)
        x$setInverseMatrix(calcInverseMatrix)
        return (calcInverseMatrix)
    }
}

