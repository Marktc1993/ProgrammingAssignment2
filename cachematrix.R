## It's a costly process to achieve matrix inversion,
## thus it's smart to cache the inverse of a matrix
## rather than compute it over and over again.
## I have two functions here for that purpose.


## This first function creates a special "matrix" object that can cache
## its inverse.
## similar to the example utilizing the mean function.
## This function sets the matrix. 1.
## Gets the matrix                2.
## Sets the inverse of the matrix 3.
## Gets the inverse               4.

makeCacheMatrix <- function(x = matrix()){
        m <- NULL
        set<- function(y){
                x<<- y
                m<<- NULL
        }
        get <- function()x
        setInverse<- function() m <<-solve(x)
        getInverse <- function()m
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## This function computes the inverse of the special matrix returned
## by makeCacheMatrix. If the inverse has already been calculated
## (and the matrix has not changed), then the cachesolve should retrieve
## the inverse from the cache.
## Getting inverse
## Check to see if the inverse has been computed,
## If so the cached value is returned
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        m<- x$getInverse()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
                
        }
        data<- x$get()
        m<-solve(data,...)
        x$setInverse(m)
        m
        
}