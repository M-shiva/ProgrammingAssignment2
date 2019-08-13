## Put comments here that give an overall description of what your
## functions do
## This function will te create special "matrix" object that will cache its inverse

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
inv <- NULL
set = function(y) {
        ## use "<<-" to assign a value to an object in an environment different fromt the current environment
        x <<- y       
        ## test
        inv <<- NULL      
}
get = function() x
setinverse = function(inverse) inv <<- inverse
getinv = function() inv
list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## This function calculates the inverse of the matrix returned by makeCacheMatrix. cacheSolve will retrive the
## inverse that has already been calculated. 
## @x: output of makeCacheMatrix()
## return: inverse of the original matrix input to makeCacheMatrix()

cacheSolve <- function(x, ...) {
        inv = x$getinv()
        
        # if the inverse has already been calculated
        if (!is.null(inv)){
                # get it from the cache and skips the computation. 
                message("getting cached data")
                return(inv)
        }
        
        # otherwise, calculates the inverse 
        mat.data = x$get()
        inv = solve(mat.data, ...)
        
        # sets the value of the inverse in the cache via the setinv function.
        x$setinv(inv)
        
        return(inv)
        ## Function completed
}
