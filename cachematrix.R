## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## COMMENTS FOR "makeCacheMatrix":
## 1) I Initialize the inverse of the matrix as NULL
## 2) In "set" function: I define the function that set the value of the matrix
##    and restart the value of the inverse as NULL
## 3) In "get" function: I define the function that get the value of the matrix
## 4) In "setinverse" function: I define the function that set the value of the
##    inverse matrix
## 5) In "Getinverse": I define the function that get the value of the inverse
##    matrix
## 6) Finally I return a list with each function as its values

makeCacheMatrix <- function(x = matrix()) {
    
    inverseMatrix <- NULL
    
    set <- function(y) {
        x <<- y
        inverseMatrix <<- NULL
    }
    
    get <- function() {
        x
    }
    
    setinverse <- function(inverse) {
        inverseMatrix <<- inverse
    }
    
    getinverse <- function() {
        inverseMatrix
    }
    
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Write a short comment describing this function

## COMMENTS FOR "cacheSolve":
## 1) Try to get the inverse matrix stored in the cache
## 2) If the inverse matrix obtained is not null then return it 
## 3) If the inverse matrix obtained is null calculate again and set the value
##    in the cache
## 4) Finally return the inverse matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    
    inverseMatrix <- x$getinverse()
    
    if(!is.null(inverseMatrix)) {
        message("getting cached data")
        return(inverseMatrix)
    }
    
    data <- x$get()
    inverseMatrix <- solve(data, ...)
    x$setinverse(inverseMatrix)
    inverseMatrix
}
