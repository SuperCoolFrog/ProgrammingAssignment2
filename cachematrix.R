## The functions in combination allow for the caching of an inverse
## of a matrix

## Accepts a matrix and returns a list for caching inverse and matrix
##
## Param
##     'x' any matrix
##
## Symbols
##     'm'          the cached value
##     'set'        assigns a new scoped matrix and resets the cache
##     'get'        returns the scoped matrix
##     'setsolve'   sets the cache
##     'getsolve'   returns the cache
##
## return list(set, get, setsolve, getsolve)
##      list with the functions to read/write to matrix and cache

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m

    list(
        set = set, get = get,
        setsolve = setsolve,
        getsolve = getsolve
    )
}


## Accepts a "cached matrix" from makeCacheMatrix and returns
## inverse from cache when exists, else calculates inverse
## and stores cache
##
## Param
##     'x' cached matrix built from makeCacheMatrix
##
## Symbols
##     'm'          the inverse of the matrix
##     'data'       data from cached matrix
## return 'm'
##      Cached value

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getsolve()
    if (!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m
}

# mat <- matrix(rnorm(16), 4, 4)
# mat <- matrix(1:4, 2, 2)

# expected <- solve(mat)

# cached_mat <- makeCacheMatrix(mat)
# cacheSolve(cached_mat) ## Solve and caches first time
# actual <- cacheSolve(cached_mat) ## retrieved from cache

# print("actual")
# print(actual)
# print("----------------")
# print("expected")
# print(expected)
# print("----------------")
# print(paste("Identical:", identical(actual, expected)))