## a pair of functions, makeCacheMatrix and cacheSolve, are (a) used to generate a lists
## of functions that will help set and get values of matrices to be inverted and then (b)
## solved while keeping track of whether the inverse of the matrix in question has previously 
## been computed and therefore cached


## makeCacheMatrix writes four other functions with the argument x, which is a matrix to be 
## inverted; two of the functions set and get the matrix values whereas the other two set 
## and get the inverse matrix values

makeCacheMatrix <- function(x = matrix()) {
        
        ## set m to be NULL and it remains NULL until an inverse matrix is solved
        m <- NULL
        
        ## a function to change the matrix entered; y is the new matrix; note 
        ## that for both x & y the operator <<- is used to update the variables 
        ## at the parent envrionment of the set function, which is the environment
        ## of makeCacheMatrix; when a new matrix is updated, m will be set to 
        ## NULL again as its inverse has not been computed
        
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        ## a function to get the matrix
        get <- function() x
        
        ## a function to set the inverse matrix; when an inverse is solved, the
        ## solution, matrix, is assigned to m at the parent environment of the 
        ## function
        setMatrix <- function(matrix) m <<- matrix
        
        ## a function to get the inverse Matrix, m, which is no longer NULL  
        getMatrix <- function() m
        
        ## a list of the four functions gets returned
        list(set = set, get = get,
             setMatrix = setMatrix,
             getMatrix = getMatrix)
        
}


## cacheSolve checks to see if the inverse of the matrix in question has been previously
## determined. If so, it simply prints the previously calculated values and if not, it
## carries out the matrix inversion computations

cacheSolve <- function(x, ...) {
        
        ## use the getMatrix function from macheCacheMatrix to update m
        m <- x$getMatrix()
        
        ## if m has been previously updated to be not NULL, message "getting cached data"
        ## is printed and the previously determined inverse of the matrix gets returned
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        ## if m is NULL, the above if statement is not executed and get function from 
        ## maakeCacheMatrix reads the matrix into the variable data
        data <- x$get()
        
        ## using the solve function, it solves the inverse and assign it to m
        m <- solve(data, ...)
        
        # use the setMatrix function from makeCacheMatrix to set the inverse of the matrix
        x$setMatrix(m)
        
        ## return a matrix that is the inverse of 'x'
        m           
}
