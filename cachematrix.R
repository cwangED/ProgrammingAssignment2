## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
makeCacheMatrix <- function(x = matrix()) {
    ## Create a matrix and cache its invelse
    
    m <- NULL ## initilize the invelse value to be cached "m"
    
    set <- function(y) { ## set value of matrix
        x <<- y ## x is the data content
        m <<- NULL ## m is the cahed value
    }
    get <- function() x ## get data content of "caheMatrix"
    setInverse <- function(solve) m <<- solve ## calcualte the inverse using solve function
    getInverse <- function() m ## read content of inverse
    
    ## the following list define the data interface of this data structure, e.g. cacheMatrix$set is the set() function above
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Write a short comment describing this function

## this function try to load the cahed inverse value at the beginning, 
##if the cached value is not NULL, it means the inverse value is already calculated and saved, then 
## it directly return it as function value. If 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()
    if(!is.null(m)) {
        message( "getting cached data" )
        return(m)
    } ## if already solved, then directly return the results
    
    data <- x$get()
    m <- solve(data) ## else, resolve the matrix
    x$setInverse(m)
    m
}