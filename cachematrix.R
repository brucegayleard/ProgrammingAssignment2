# makeCacheMatrix: Takes a matrix as input and creates a special "cache matrix" object that
#                  caches its inverse and tracks changes to the matrix last inversed.

makeCacheMatrix <- function(inMatrix = matrix()) {
    # initialise inverse & changed values
    inverseMatrix <- NULL
    changedMatrix <- TRUE
    # set will set the matrix and initialise inverse & changed values
    set <- function(setMatrix) {
        inMatrix <<- setMatrix
        inverseMatrix <<- NULL
        changedMatrix <<- TRUE
    }
    # get returns the matrix
    get <- function() inMatrix
    # setinverse computes the inverse, caches it & sets the changed value
    setinverse <- function() {
        inverseMatrix <<- solve(inMatrix)
        changedMatrix <<- FALSE
    }
    # getinverse returns the cached inverse
    getinverse <- function() inverseMatrix
    # getchanged returns the value of changed
    getchanged <- function() changedMatrix
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse,
         getchanged = getchanged)
}


# cacheSolve: Takes as input a special "cache matrix" object created by makeCacheMatrix.
#             If the matrix has not changed and the inverse has been cached, then cacheSolve
#             retrieves the cached inverse, otherwise it computes the inverse and caches it.

cacheSolve <- function(cacheMatrix, ...) {
    # check matrix data has not changed
    chg <- cacheMatrix$getchanged()
    if (!chg) {
        message("matrix data not changed")
        # matrix data unchanged, so get the cached inverse
        cacheInverse <- cacheMatrix$getinverse()
        if(!is.null(cacheInverse)) {
            # cached inverse is not null, so return it
            message("getting cached inverse")
            return(cacheInverse)
        }
    }
    # set & cache the inverse
    cacheMatrix$setinverse()
    # return the just cached inverse
    cacheMatrix$getinverse()
}
