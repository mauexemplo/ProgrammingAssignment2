## This code is related to the Programming Assignment 2 of the Coursera
## R Programming course, as presented by Roger Peng on July 2015.
##
## Written by Daniel Fonseca on July 23, 2015.
##
## These functions create an object to hold a matrix and its cached inverse,
## and calculate and store that inverse on request.

## makeCacheMatrix creates an object to store both a matrix and its inverse,
## with getters and setters for each of them.

makeCacheMatrix <- function( x = matrix() )
{
    s <- NULL
    
    list(
        set = function(y)
        {
            x <<- y
            s <<- NULL
        },
        
        get = function() {x},
        
        setSolve = function(t) {s <<- t},
        
        getSolve = function() {s}
    )
}


## cacheSolve works with a matrix object created by makeCacheMatrix, and
## returns the inverse of that matrix, either by reading the cached value
## or by calculating (and updating) it.

cacheSolve <- function( x, ... )
{
    s <- x$getSolve()
    
    if( is.null(s) )
    {
        ## There is no cached value, calculate and store
        s <- solve( x$get() )
        x$setSolve(s)
    }
    else
        message("Cached value - no calculation")
    
    s
}