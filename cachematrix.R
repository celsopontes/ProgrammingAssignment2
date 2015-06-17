## Assignment 2: Functions that cache and compute the inverse of a matrix. 

## Creates a cacheable matrix to use as input of cacheSolve() function 
## that sets and gets the cached values

makeCacheMatrix <- function(input.matrix = matrix()) {

        if (!is.matrix(input.matrix)){
                stop("Matrix not found. You need to enter the matrix!")
        }
        
        inverse.matrix <- NULL
        
        set <- function(x){
                input.matrix <<- x
                inverse.matrix <<- NULL
        }
        
        get <- function() input.matrix
        set.inverse <- function(inverse) inverse.matrix <<-inverse
        get.inverse <- function() inverse.matrix
        
        list(   set = set,
                get = get,
                set.inverse = set.inverse,
                get.inverse = get.inverse)                
}

## The function computes the inverse of the cacheable matrix returned by
## makeCacheMatrix() above. If the inverse has already been calculated and
## there's no change in the matrix then the cacheSolve() returns the cached inverse

cacheSolve <- function(input.matrix, ...) {
        inverse.matrix <- input.matrix$get.inverse()
        
        if (!is.null(inverse.matrix)){
                message("Getting cache data")
                return(inverse.matrix)
        }
        inv.mtx <- input.matrix$get()
        inverse.matrix <- solve(inv.mtx)
        input.matrix$set.inverse(inverse.matrix)
        inverse.matrix
}