## ASSIGNMENT: CACHING THE INVERSE OF A MATRIX

# This assignment is all about writing R functions to 
# improve efficiency by caching (temporarily storing 
# the result of some calculation somewhere else). 

# Matrix inversion is usually a costly computation and 
# there may be some benefit to caching the inverse of a
# matrix rather than compute it repeatedly. 
# Your assignment is to write a pair of functions that
# cache the inverse of a matrix.

# Write the following functions:

# 1) makeCacheMatrix: This function creates a special
# "matrix" object that can cache its inverse.

# 2) cacheSolve: This function computes the inverse of
# the special "matrix" returned by makeCacheMatrix 
# above. If the inverse has already been calculated 
# (and the matrix has not changed), then the 
# cachesolve should retrieve the inverse from the cache.

# Computing the inverse of a square matrix can be done with
# the solve function in R. For example, if X is a square 
# invertible matrix, then solve(X) returns its inverse.
# For this assignment, assume that the matrix supplied 
# is always invertible.

# Let's get cracking!:

# 1) This function makes a special "matrix" object that can cache it's inverse:
makeCacheMatrix <- function(x = matrix()) { ## Define the 'makeCacheMatrix' function (an empty matrix)
        m <- NULL       ## initialize the variable 'm' to store the cached inverse (NULL means that no inverse has been calculated yet)
        set <- function(y) {    ## set the value of the matrix in parent environment
                x <<- y         ## assign 'x' to value in parent environment (modify variable in the parent environment)
                m <<- NULL      ## resets cached inverse to NULL
        }
        get <- function() x    ## retrieve the value of the matrix
        setinverse <- function(inverse) m <<- inverse ## set the value of the cached inverse
        getinverse <- function() m      ## retrieve the value of the cached inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)   ## return a list containing the four functions
} 


# 2) This function computes the inverse of the special matrix 'makeCacheMatrix' (above)
cacheSolve <- function(x, ...) {   ## define the 'cacheSolve' function 
        m <- x$getinverse()     ## get cached inverse
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <-x$get()          ## get matrix data
        m <- solve(data, ...)   ## calculate the inverse of the matrix
        x$setinverse(m)         ## set cached inverse
        m
}

# run a test:
my_test_matrix <- matrix(c(1, 2, 3, 4), nrow = 2, ncol = 2)  ## create test matrix
cached_matrix <- makeCacheMatrix(my_test_matrix)  ## create special matrix object to store cached matrix
inverse_matrix <- cacheSolve(cached_matrix)     ## calculate inverse and cache
inverse_matrix2 <- cacheSolve(cached_matrix)    ## verify cached result
# output shows: "getting cached data"