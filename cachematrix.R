## Put comments here that give an overall description of what your
## functions do

#This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    #local variable to store cache
    mtrx_cache <- NULL
    #func to set new matrix data
    set <- function(y) {
        x <<- y
        #delete cache
        mtrx_cache <<- NULL
    }
    #func to get matrix
    get <- function() {x}
    #func to set cache value
    setcache <- function(cache) {mtrx_cache <<- cache}
    #funt to get cache
    getcache <- function() {mtrx_cache}
    #inner func to solve
    solve2 <- function() {
        ## if no cache solve to get matrix that is the inverse of 'x' 
        if(is.null(mtrx_cache)) {
            mtrx_cache <<- solve(x)
        }
    }
    list(set = set, get = get,
         setcache = setcache,
         getcache = getcache, solve2 = solve2)
}


#This function computes the inverse of the special "matrix" returned by makeCacheMatrix above
#If the inverse has already been calculated (and the matrix has not changed),
#then the cachesolve should retrieve the inverse from the cache
#outer func
cacheSolve <- function(x, ...) {
    mtrx_cache <- x$getcache()
    #if cache already exists
    if(!is.null(mtrx_cache)) {
        message("getting cached data")
        return(mtrx_cache)
    }
    #else get matrix data
    data <- x$get()
    message("solving")
    ## Return a matrix that is the inverse of 'x'
    mtrx_cache <- solve(data, ...)
    #save to cache
    x$setcache(mtrx_cache)
    mtrx_cache
}
