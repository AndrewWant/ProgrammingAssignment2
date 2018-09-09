## These functions take an matrix and inverts it, caching the output. 

## This function returns a list of functions, and links a global variable: 
## inverse_matrix

makeCacheMatrix <- function(mtx = matrix()) {
              inverse_matrix <<- NULL
              get <- function() mtx
              set <- function(y) {
                  mtx <<- y
                  inverse_matrix <<- NULL
              }
              list(get = get, set = set)
}


## This function takes a list object containing a matrix as its argument, gets 
## the matrix and inverts it, writing this back to the global environment

cacheSolve <- function(mtx, ...) {
        ## Return a matrix that is the inverse of 'x'
    if(is.null(inverse_matrix) && nrow(mtx) == ncol(mtx)){
        matrix_for_solve <- mtx$get()
        inverse_matrix <<- solve(matrix_for_solve)
        message("Matrix has been inverted")
    } else if (nrow(mtx) != ncol(mtx)){
        message("Matrix is not square")
    }else {
        message("Inverse matrix already exists")
        inverse_matrix
    }
}


## Testing ##

test_m <- matrix(c(0, -1, 1, 0), 2)
test_m

m <- makeCacheMatrix(test_m)
if(is.null(inverse_matrix)){
    message("inverse_matrix has been created and is NULL")
}

cacheSolve(m)
inverse_matrix

m$set(test_m)
if(is.null(inverse_matrix)){
    message("inverse_matrix has been created and is NULL")
}

