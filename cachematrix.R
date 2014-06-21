## The functions below take advantage of the lexical scoping rules of R language
## to cache the results of potentially time-consuming operations. 
##
## This is possible because a function closure includes references to all non-local variables
## that a function uses. As long as the function objects are live, these non-local variables 
## persist in memory.
##
## This is similar annonymous methods and lambda expressions in C#
##
## For more information on function closure, see the link below:
## http://en.wikipedia.org/wiki/Closure_%28computer_science%29
##
##
## To use the functions, one should initialise a list of function objects by
## calling makeCacheMatrix, passing the matrix to be inversed as the argument
## To get the inverse of the matrix, pass this list object to function cacheSolve. 
## E.g.
## m = matrix(rnorm(1000000), 1000, 1000)
## cache <- makeCacheMatrix(m)
## inverse <- cacheSolve(cache)
## inverse <- cacheSolve(cache)
##
## Note that the first time cacheSolve function is called, it carries out the inverse 
## operation. The second time it is called, it retrieves the result that is cached.
##
## To understand the differences in computation time, replace
## inverse <- cacheSolve(cache)
## with
## system.time(inverse <- cacheSolve(cache))
##
## matrix can be reset by using set.matrix function in the list object. E.g.
## cache$set.matrix(m)
##

## makeCacheMatrix
## 
## This function creates a list object containing 4 functions together with the cache
## variables in the function closures to enable the cacheSolve function to store
## the inverse matrix results in the memory cache.
##
## argument:
## x:               the initial value of the matrix
##
## return value:
## a list of 4 functions:
## get.matrix():    return the matrix in the cache
## set.matrix(m):   set the matrix in the cache
## get.solve():     get the inverse of the matrix stored in the cache
## set.Solve(s):    set the inverse of the matrix in the cache

makeCacheMatrix <- function(x = matrix()) {
    ## set the cached result as NULL as it hasn't been initialised yet
    s <- NULL
    
    ## set.matrix function sets the matrix to be inversed
    set.matrix <- function(m) {
        x <<- m
        s <<- NULL ## reset the cached result
    }
    
    ## get.matrix function returns the matrix object
    get.matrix <- function() x
    
    ## set.solve function sets the cached result
    set.solve <- function(solve) s <<- solve
    
    ## get.solve function returns the cached result
    get.solve <- function() s
    
    ## returning a list of the 4 functions
    list(set.matrix = set.matrix,
         get.matrix = get.matrix,
         set.solve = set.solve,
         get.solve = get.solve)
}

## cacheSolve
## 
## This function uses the list object created by makeCacheMatrix function
## to cache inverse matrix results in the memory
## 
## Arguments:
## x:               the list object returned by makeCacheMatrix function
## ...:             further arguments to be massed to function
##
## Return value:
## inverse of the matrix contained in x

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    ## retrieve the cached result
    s = x$get.solve()
    ## test to see if the cached result has been initialised yet
    if (!is.null(s)) {
        ## if the cached result has been initialised, return its value
        message("getting cached result")
        return(s)
    }
    ## if the cached result has not been initialised, use solve function to
    ## compute the inverse of the matrix and store it in the cached result
    message("initialising cached result")
    m <- x$get.matrix()
    s <- solve(m, ...)
    x$set.solve(s)
    s   
}
