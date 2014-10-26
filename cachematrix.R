## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv_mat <- NULL
        set_mat <- function(y) {
                x <<- y
                inv_mat <<- NULL
        }
        get_mat <- function() x
        set_cache_inverse <- function(inverse_input) inv_mat <<- inverse_input
        get_cache_inverse <- function() inv_mat
        list(set_mat=set_mat, get_mat=get_mat, set_cache_inverse=set_cache_inverse, get_cache_inverse=get_cache_inverse)
}


## Write a short comment describing this function
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## get inverse from cache if it exists
        inverse_x <- x$get_cache_inverse()
        

        if(!is.null(inverse_x)) {
                message("This is cached data")
                return(inverse_x)
        }
        
        ## compute and Return a matrix that is the inverse of 'x'
        
        cached_input_data <- x$get_mat()
        inverse_x <- solve(cached_input_data)
        x$set_cache_inverse(inverse_x)
        return(inverse_x)
        
}
