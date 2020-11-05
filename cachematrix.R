## Put comments here that give an overall description of what your
## functions do:
## makeCacheMatrix takes a matrix as an input and stores it and its inverse
## in a list
## 

## Write a short comment describing this function:
## makeCacheMatrix basically creates a list that has a matrix and can contain its inverse
## 
## cacheSolve takes the list above and checks if there is and inverse already
## cached. If there is it just prints the cached value.If not it calculates the
## inverse and writes to the aforementioned list + gives you the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setinv <- function(inverse2) inverse <<- inverse2
    getinv <- function() inverse
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

## Write a short comment describing this function:
## If the calculation already been done, it just gives that inverse matrix
## If not, caclculates it, caches it , then gives that inverse matrix

cacheSolve <- function(x, ...) {
    inverse <- x$getinv()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data)
    x$setinv(inverse)
    inverse
    ## Return a matrix that is the inverse of 'x'
}
       

