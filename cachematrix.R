
## a pair of functions that cache the inverse of a matrix
##Description:
## You pass in a matrix and if the values have been 
##  passed in before it will not re-calculate the values
##  it the values are new the inverse will be calculated
##  stored in the cache to speedup the calculations if
##  the same values are passed in again.
##Example use:
## x <- makeCacheMatrix(matrix( c(2, 4, 8, 9), nrow=2, ncol=2))
## cacheSolve(x)



## function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(obj_matrix) {
    ## clear staging variable
    matrix_staging <- NULL 
    
    ## set new matrix values
    set_matrix <- function(new_matrix) { 
        ## overwrite existing matric with new imput
        obj_matrix <<- new_matrix 
        ## clear staging variable
        matrix_staging <<- NULL
    } 
    
    ## return exisitng values
    get_matrix <- function() obj_matrix
    
    ## set new matrix values in staging valiable
    set_matrix_inverse <- function(new_matrix_inverse) matrix_staging <<- new_matrix_inverse
    
    ## return exisitng inversed values
    get_matrix_inverse <- function() matrix_inverse
    
    ## return value of the makeCacheMatrix function and expose access mehtods 
    list(set_matrix = set_matrix
         ,get_matrix = get_matrix 
         ,set_matrix_inverse = set_matrix_inverse 
         ,get_matrix_inverse = get_matrix_inverse ) 
}



## function computes the inverse of the special "matrix" returned by makeCacheMatrix
cacheSolve <- function(my_obj_matrix, ...) { 
    
    ##retruns the inverse values from matrix that is passed in
    matrix_inverse <- my_obj_matrix$get_matrix_inverse()
    
    ## if values exist alerady return as is (exit function)
    if(!is.null(matrix_inverse)) { 
        ##inform user
        message("getting cached data")
        ##exit function returning the unchanged values
        return(matrix_inverse) 
    } 
    
    ## no values exists and get matrix to perform the calculations on 
    data <- my_obj_matrix$get_matrix() 
    
    ## compute the inverse on matrix 
    ##matrix_inverse <- solve(data)
    z <- list(a = data )
    matrix_inverse <- lapply(z, solve, ...)
    
    ## over witer the matrix values (cache the matrix)
    my_obj_matrix$set_matrix_inverse(matrix_inverse)
    ##return the caching matrix
    matrix_inverse 
}
