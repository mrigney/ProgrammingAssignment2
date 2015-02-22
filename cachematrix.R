## These functions will take an matrix and solve for its inverse. The inverse is
## then stored. Then, on future runs, if the inverse already exists and no new
## matrix has been given, the program returns the cached matrix instead of 
## solving again

## Takes a matrix as an argument. Returns four functions as a list. This in
## essence acts as a constructor function or a class definition.

makeCacheMatrix <- function(Matrix = matrix()) {
      InverseMatrix <- NULL
      SetValues <- function(y){
            Matrix <<- y
            InverseMatrix <<- NULL
      }
      get <- function() Matrix
      SetInverse <- function(SolvedMatrix){
            InverseMatrix <<- SolvedMatrix
      }
      
      GetInverse <- function() InversetMatrix
      
      list(SetValues = SetValues, get = get,
           SetInverse = SetInverse,
           GetInverse = GetInverse)      

}


## This function takes an argument that is a list (from makeCacheMatrix) and 
## then either solves for a new matrix if it has been defined, or returns the
## cached inverse of a previously defined matrix.

cacheSolve <- function(Matrix, ...) {
        ## Return a matrix that is the inverse of 'x'
      InverseMatrix <- Matrix$GetInverse()
      if(!is.null(InverseMatrix)) {
            message("getting cached data")
            return(InverseMatrix)
      }
      data <- Matrix$get()
      InverseMatrix <- solve(data)
      Matrix$SetInverse(InverseMatrix)
      InverseMatrix
}
