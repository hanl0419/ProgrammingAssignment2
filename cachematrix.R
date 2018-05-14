## These functions were created to cache the Inverse of a Mmtrix.


## Creates a special "matrix" that cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    i<- NULL 
    ##set the value of the matrix
    set<- function(y){
        x<- y
        i<- NULL
    }
    ##get the value of the matrix
    get<- function() x
    #set the value of the inverse
    setiv<- function(inverse) i <<- inverse
    #get the value of the inverse
    getiv<-function() i
    list(set=set, get= get, setiv= setiv, getiv= getiv)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
##then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i<- x$getiv()
    ##frist check if the inverse is already calculated.
    if (!is.null(i)){
        message("getting cached data")
        return(i)
    }
    data<- x$get()
    ##calculate the inverse with solve function.
    i<- solve(data,...)
    x$setiv(i)
    return(i)
}
