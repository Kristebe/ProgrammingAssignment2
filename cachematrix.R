## Pair of functions that cache the inverse of a matrix

## makeCacheMatrix: 
## creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y = matrix()) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInv <- function(inverse) inv <<- inverse
    getInv <- function() inv
    list(set = set,
         get = get,
         setInv = setInv,
         getInv = getInv)
}

## cacheSolve:
## Computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
    inv <- x$getInv()
    
    # Retrieve inverse from the cache if not NULL
    if (!is.null(inv)) {
        message("Getting inverse from cache.")
        return (inv)
    }
    # Calculate inverse and set it in the cache 
    message("Calculating inverse.")
    inv <- solve(x$get(), ...)
    x$setInv(inv)
    inv
}

## EXAMPLE
#A = matrix(c(1,2,3,4), nrow=2, ncol=2)
#print(A)
#print(solve(A))
#Am <- makeCacheMatrix(A)
#print(cacheSolve(Am))
#print(cacheSolve(Am))

