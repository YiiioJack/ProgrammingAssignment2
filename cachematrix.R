# If A*B = B*A = I, 
# The invertible matrix of A is B (= A^-1)

#------------------------------------------------------------------------------

# makeCacheMatrix:
## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(m = matrix()) {
    i <- NULL
    
    set <- function(matrix) {
        m <<- matrix
        i <<- NULL
    }
    
    get <- function() {m}
    
    set_Inverse <- function(inverse) {i <<- inverse}
    
    get_Inverse <- function() {i}
    
    list(set = set,
         get = get,
         set_Inverse = set_Inverse,
         get_Inverse = get_Inverse)
}


# cacheSolve: 
## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. 

## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    # Return a matrix that is the inverse of 'x'
    i <- x$get_Inverse()
    
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    
    data <- x$get()
    i <- solve(data, ...)
    x$set_Inverse(i)
    i
}

#------------------------------------------------------------------------------

# Example 1:

A <- matrix(c(-1, -3, 2, 4), 2, 2)
X <- makeCacheMatrix(A)
B <- cacheSolve(X)

A
B
A %*% B


# Example 2:

m1 <- matrix(rnorm(16),4,4)
Y <- makeCacheMatrix(m1)
m2 <- cacheSolve(Y)

m1
m2
m1 %*% m2
