## makeCacheMatrix function creates a "matrix" object that can cache its inverse,
## it takes a matrix as an input and sets it inverse,
## then returns the same through get function, 
## the matrix object can cache its own object,

  makeCacheMatrix <- function(x = matrix()) { ## defining the argument with default "matrix"
    inv <- NULL                               ## initialize inverse (inv) as NULL; it will hold value of invertible matrix
    setmatrix <- function(y) {                ## setmatrix function defined to assign a value to an object 
      x <<- y                                 ## in an environment different from the current environment
      inv <<- NULL                            ## reset inv to NULL if it is a different matrix
    }
    getmatrix <- function() x                 ## getmatrix function defined to return value of the matrix when called
    
    setinverse <- function(inverse) inv <<- inverse  ## set the value of invertible matrix in
    getinverse <- function() inv                     ## gets the value of invertible matrix
    list(setmatrix = setmatrix, getmatrix = getmatrix, setinverse = setinverse, getinverse = getinverse)  ## required to refer to functions defined

}


## cacheSolve function takes the matrix output from makeCacheMatrix function 
## as an input and returns the inverse of the matrix (if not already calculated)
## If inverse matrix from makeCacheMatrix((matrix) is empty, it gets the original matrix 
## and calculates the inverse and returns the same
## If inverse matrix from makeCacheMatrix((matrix) has a value in it 
## it returns a message  "Getting Cached Inverted Matrix" 
## and returns the cached object which is the inverted matrix
   
  cacheSolve <- function(x, ...) {               ## Returns a matrix that is the inverse of 'x'
                                                
    inv <- x$getinverse()                        ## gets the value of the invertible matrix from the makeCacheMatrix function
    if(!is.null(inv)) {                          ## if inverse matrix is not NULL
      message("Getting Inverted Matrix")         ## message displayed: Getting Inverted Matrix 
      return(inv)                                ## returns the value of inverted matrix
    }
    
                                                 ## if value of the invertible matrix is NULL
    MatrixData <- x$getmatrix()                  ## get the original Matrix value 
    inv <- solve(MatrixData, ...)                ## use solve function to inverse the matrix
    x$setinverse(inv)                            ## set the inverse of the matrix 
    return(inv)                                  ## return the inverted matrix
  }