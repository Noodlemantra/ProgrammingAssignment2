## Two functions that are used for calcuating the inverse of a matrix
## Since it can be a costly computation, it's faster to retrieve the inverse of a matrix if it's previously been calculated.

## This function creates a special "matrix" object that can cache its inverse
## The function builds a set of functions and returns the functions within a list to the parent environment 

makeCacheMatrix <- function(x = matrix()) {
        Inv <- NULL
        set <- function(y) {
                x <<- y
                Inv <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) Inv <<- solve
        getsolve <- function() Inv
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above 
## If this is the first time that the inverse needs to be calculated, Inv is set to NULL so the function calculates it and prints the inverse 
## If it has been previously calculated, returns the message "getting cached data" and retrieves the inverse from the cache

cacheSolve <- function(x, ...) {
        Inv <- x$getsolve()
        if(!is.null(Inv)) {
                message("getting cached data")
                return(Inv)
        }
        data <- x$get()
        Inv <- solve(data, ...)
        x$setsolve(Inv)
        Inv
}

## This part is to show how the functions work
# The matrix is a 2x2 matrix, so it's small, but shows that the first call calculates and returns the inverse while the second call gets the inverse from cache
# run the next lines one by one t

A <- makeCacheMatrix(matrix(1:4, nrow=2,ncol=2)) #creates the "environment" makeCacheMatrix which has the matrix x, Inv (inverse set to NULL) and 4 functions: get, set, getsolve, setsolve
A$get() #prints the original matrix 
A$getsolve() # shows that the inverse matrix is not calculated yet, so the inverse is set to NULL 
cacheSolve(A)          # first run - calculates the inverse and prints it
cacheSolve(A)          # second run - since the inverse is allready in cache, prints the message "getting cached data" and then prints the inverse 

