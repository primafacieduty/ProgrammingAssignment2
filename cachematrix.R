## Matrix algebra computations, such as the inverse of a matrix are often
## expensive.  The functions below make use of cache memory to compute
## and store ain inverse matrix



makeCacheMatrix <- function(x = matrix()) { # defines the argument as a matrix
    inverse <- NULL # initalize inverse as NULL
    set <- function(y){ # define set function; assigns new value of matrix in parent environment
        x <<- y # value of matrix in parent environment
        inverse <<- NULL # if a matrix already exists reset inverse to NULL
    }
    get <- function() x # define 'get' - returns value of matrix argument
    setinv <- function(CompleteMatrix) inverse <<- CompleteMatrix # defines value of 'inverse' in parent environment
    getinv <- function() inverse # retrieves the value of inverse
    list(set = set, get = get, setinv = setinv, getinv = getinv) # creates a list for the functions
}


## This functions takes the output of "makeCacheMatrix"
## Function returns the inverse matrix
## If inverse already stored in cache function retrieves the existing matrix

cacheSolve <- function(x, ...) { # argument of function is the output of makeCacheMatrix
    inv <- x$getinv() # calls value of function getinv
    if(!is.null(inv)){ # if inv is not NULL then function returns a message and the cached matrix
        message("Returning cached data")
        return(inv)
    }
    data <- x$get() # if inv is NULL then function retrieves orignial matrix
    inv <- solve(data) # inverts the matrix; for solve function to work matrix must be square
    x$setinv (inv) # sets the inverse matrix into cache
    inv # Return a matrix that is the inverse of 'x'
}
