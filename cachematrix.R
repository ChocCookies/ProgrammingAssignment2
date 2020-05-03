
# Write a short comment describing this function

# makeCacheMatrix creates a 'special' matrix object, 
# which is a list with 4 functions : get,set,getInv,setInv
# get retrieves the value of the matrix
# set sets the value of the matrix
# getInv retrieves the inverse of the matrix
# setInv sets the inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInv <- function(inv) m <<- inv
    getInv <- function() m
    list(set = set, get = get, setInv = setInv, getInv = getInv)
}

## Write a short comment describing this function

# cacheSolve returns the inverse of the matrix ,and caches its value.
# cacheSolve takes the list created in the makeCacheMatrix function and 
# returns the value of its inverse using the solve() function.
# if the inverse is already cached,
# then the calculation is skipped and the cached value is returned.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
        
    m <- x$getInv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data,...)
    x$setInv(m)
    m
}
