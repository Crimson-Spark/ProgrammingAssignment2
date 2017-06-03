# DESCRIPTION (OVERALL): These Functions that I have created will compute the inverse of a Matrix,
# given that the matrix object is created using the makeCacheMatrix-created object and passed into the 
# cacheSolve function, and return it to the user.  If the inverse of the matrix has already been
# calculated, the cacheSolve function will return this value from the cache rather than compute it
# again.  


# Description (MAKECACHEMATRIX): This function takes a matrix and assigns it an inverse value of NULL.
# This function then assigns the object a pair of set and get functions for the matrix and its
# inverse.  Because there are functions attached to the object created with this function, the 
# original function is kept in memory and further calls can be made to objects in its environment.

makeCacheMatrix <- function(m = matrix()) { 
        
        #Initialize the inverse of the matrix as NULL
        inverse <- NULL
        
        # Set the value of the matrix, overriding the previous matrix and inverse
        set <- function(newMatrix) {
                m <<- newMatrix
                inverse <<- NULL
        }
        
        # Get the value of the matrix
        get <- function() {
                m
        }
        
        # Set the value of the inverse of the matrix
        setInverse <- function(inverse) {
                i <<- inverse
        }
        
        # Get the value of the matrix inverse
        getInverse <- function() {
                i
        }
        
        # Set a list containing these nested functions so they can be accessed later with $
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}



# DESCRIPTION (CACHESOLVE): This function takes an argument in the form of an object created using
# the CREATECACHEMATRIX function.  If the object passed to it is not of this type, it will throw an error.
# This is because the function calculates the inverse of the matrix created from the special cached
# matrix function above, which is kept in memory and therefore can be checked by further functions.
# If the inverse of the object is null, then its inverse is calculated and assigned to the inverse
# value contained in the createMatrixFunction.  If the value is not null and has already been calculated,
# it is simply retrieved from memory.

cacheSolve <- function(m, ...) {
        
        # Checks the object that is input for the value of its inverse (pointed to makeCacheMatrix)
        inverse <- m$getInverse()
        
        # Checks if this matrix created by makeCacheMatrix has already had its inverse computed.  If so, return the computed value.
        if(!is.null(inverse)) {
                message("Returning Matrix Inverse")
                inverse
        }
        
        # Set a value equal to the entire matrix (obtained from the get() nested function) from the fed-in object
        intermediate <- m$get()
        
        # Sets a temporary inverse value equal to the inverse of the above matrix (NOTE: ginv is from the MASS Package.  It calculates a Matrix's Inverse)
        inverse <- ginv(intermediate, ...)
        
        # Set the inverse of the original makeCacheMatrix object to the computed inverse
        m$setInverse(inverse)
        
        # Returns inverse value
        inverse
}
