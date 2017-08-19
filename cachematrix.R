## Put comments here that give an overall description of what your
## functions do

## "makeCacheMatrix: it creates a special matrix object that can cache its inverse.
## "cacheSolve: this function computes the inverse of the special matirx returned by makeCachMatrix above.
## If the inverse has already been calculated and the matrix has not changed, 
## then the cachSolve should retrieve the inverse from the cache

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {  
    inv <- NULL           ## set the matrix inverse is NULL
    set <- function(y){   ## if user want to reset matrix
        x <<- y           ## reassign "new" matrix to x
        inv <<- NULL      ## store matrix in cache
    }
    get <- function() x
    setInverse <- function(solveMatrix) inv <<- solveMatrix  ##set inverse matrix
    getInverse <- function() inv                             ##get inverse matrix
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)  ##create list of functions
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {      ## return a matrix that is the inverse of 'x'
    inv <- x$getInverse() 
    if(!is.null(inv)){                ## to check if the user had calculated the same matrix before
        message("getting cached data")
        return(inv)                   ## return the old result, inv, without recalculating  
    }
    data <- x$get()                   ## get the uncalculated matrix
    inv <- solve(data)                ## calculate the inverse of the matrix
    x$setInverse(inv)                 ## reassign inverse matrix
    inv                               ## print the inverse matrix
}