## Programming Assignment 2 for R Programming 

## I wish to acknowledge the use of ideas and code provided by the course resources
## -------------------------------------------------------------------------------------------------------
## The purpose of makeCacheMatrix is to create a special matrix object that
## can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {         
  
      inv <- NULL
      set <- function(y) {
            x <<- y                       ## access value of x in different environment outside of current
            inv <<- NULL                  
      }
      get <- function() x                              ## give access to the matrix object
      setinverse <- function(solve) inv <<- solve      ## the inverse is found using 'solve' function in R
      getinverse <- function() inv                     ## 'inv' will represent the inverse of 'x'
                                                       ## finally, return the list of parameters
      list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## ------------------------------------------------------------------------------------------------------

## Function cacheSolve will compute the inverse of the special matrix returned by makeCacheMatrix. 
## It will check to see if the inverse has already been calculated, and that the matrix
## has not changed. It should then retrieve the inverse from the cache.

## I do not check if the matrix is the same one that is cached, so that is a problem in my submission
## There is no error to handle the singular matrix situation in code

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
      inv <- x$getinverse()                           ## probe the cache
      if(!is.null(inv))  {                            ## if the cached item is available...
            message("Getting cached data")            ## ...then fetch it
            return(inv)
      }
      data <- x$get()                                 ## otherwise, we calculate the inverse of 'x' matrix
      inv <- solve(data, ...)
      x$setinverse(inv)
      inv
}
