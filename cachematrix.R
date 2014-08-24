#################################################################################################
# FUNCTION:     makeCacheMatrix:                                                                #
# DESCRIPTION:  This function creates a special "matrix" object that can cache its inverse.     #
#===============================================================================================#
# FUNCTION:     cacheSolve:                                                                     #
# DESCRIPTION:  This function computes the inverse of the special "matrix" returned by          #
#               makeCacheMatrix above.                                                          #
#               If the inverse has already been calculated (and the matrix has not changed)     #
#               then the cachesolve should retrieve the inverse from the cache.                 #        
#                                                                                               #
#               The definition of a matrix's inverse is that the product of the matrix and its  #
#               inverse is the identity matrix, if the inverse exists                           #
#               http://www.r-bloggers.com/quick-review-of-matrix-algebra-in-r/                  #
#===============================================================================================#
# UNIT_TEST:   Test function cacheSolve computes inverse of matix                               #
# DESCRIPTION: Create a test matrix and determine inverse                                       # 
# CODE:                                                                                         #
## Create test harness matrix                                                                   #
#> matrix_test_harness <- matrix(c(0, 2, 1, 0), nrow = 2, ncol = 2, byrow = TRUE)               #
#                                                                                               #
## Determine inverse                                                                            #
#> solve(matrix_test_harness)                                                                   #
#                                                                                               #
## Test computation                                                                             #
#> solve(matrix_test_harness) %*% matrix_test_harness                                           #
#                == diag(nrow = nrow(matrix_test_harness), ncol = ncol(matrix_test_harness))    #
#                                                                                               #
## Load function makeCacheMatrix                                                                #
#> function_test_harness <- makeCacheMatrix(matrix_test_harness)                                #
#                                                                                               #
## Test function cacheSolve                                                                     #
#> cacheSolve(function_test_harness)                                                            #
#===============================================================================================#



#===============================================================================================#
# FUNCTION:     makeCacheMatrix:                                                                #
# DESCRIPTION:  This function creates a special "matrix" object that can cache its inverse.     #
#===============================================================================================#
makeCacheMatrix <- function(x = matrix()) {
        
        #Set the inverse value not to persist
        inverse_cached_value <-NULL
               
        #set the value of the matrix
        set <- function(my_Matrix) {
                x <<- my_Matrix
                inverse_cached_value <<- NULL
        }
        
        #get the value of the matrix
        get <- function() x
        
        #set the value of the inverse
        set_Inverse <- function(inverse) inverse_cached_value <<- inverse
        
        #get the value of the inverse   
        get_Inverse <- function() inverse_cached_value

        ## Return a list of the methods
        list(set = set, get = get,
             set_Inverse = set_Inverse,
             get_Inverse = get_Inverse)
}




#===============================================================================================================#
# FUNCTION:     cacheSolve:                                                                                     #
# DESCRIPTION:  Returns a matrix that is the inverse of 'x' only if identical matrix is not already cached      #
#===============================================================================================================#
cacheSolve <- function(x, ...) {
        
        # Try load the cached matrix
        m_cache_matrix <- x$get_Inverse()
        
        # If the object has data return the cached data and exit the function
        if(!is.null(m_cache_matrix)) {
                message("Found identical cached data, no further calculation required")
                return(m_cache_matrix)
        }
        
        # If there is no cached data get the supplied data
        # and calculate the inverse of the supplied matrix 
        supplied_data <- x$get()
        m_cache_matrix <- solve(supplied_data)
        
        ## Set the inverse to the object
        x$set_Inverse(m_cache_matrix)
        
        ## Return the matrix
        m_cache_matrix
}
        
