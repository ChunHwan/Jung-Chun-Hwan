# Matrix inversion is usually a costly computation and 
# there may be some benefit to caching the inverse of a matrix 
# rather than compute it repeatedly 

# This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(para_x = matrix()) {
        v_inverse <- NULL

	  # Set & Get para_x
        Func_set_para_x <- function(para_y) {
                para_x <<- para_y
                v_inverse <<- NULL
        }
        Func_get_para_x <- function() { para_x }

        # Set & Get Inversing Value of x which is v_inverse.
        Func_set_Inverse_x <- function(argu_inv_x) { v_inverse <<- argu_inv_x }
        Func_get_Inverse_x <- function(){ v_inverse }

        list(set_x = Func_set_para_x, 
 		 get_x = Func_get_para_x,
             set_inverse_x = Func_set_Inverse_x,
             get_inverse_x = Func_get_Inverse_x)
}

# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed),
# then the cachesolve should retrieve the inverse from the cache.
# Computing the inverse of a square matrix can be done with the solve function

cacheSolve <- function(obj_MCM, ...) {
        # return a Matrix of inverse of Para_x
        temp_inv <- obj_MCM$get_inverse_x()
        if(!is.null(temp_inv)) {
                message("getting cached data")
                return(temp_inv)
        }
        Temp_x <- obj_MCM$get_x()
        temp_inv <- solve(Temp_x, ...)
        obj_MCM$set_inverse_x(temp_inv)
        temp_inv
}

# test Procedure
#
#

test_para_matrix <- matrix(1:4, 2, 2)
test_matrix <- makeCacheMatrix(test_para_matrix)
test_matrix$get_x()
test_matrix$get_inverse_x()
cacheSolve(test_matrix)
test_matrix$get_inverse_x()
