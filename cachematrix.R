## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
       
       inv_Matrix <- matrix()
       
       set <- function(y = matrix()) {
              x <<- y              
              inv_Matrix <<- NULL              
       }

       get <- function() x
       
       
       #Set & Get for cached inverse matrix
       setInvMatrix <- function(iMatrix) inv_Matrix <<- iMatrix
       
       getInvMatrix <-function() inv_Matrix
       
       list(set=set, get=get, setInvMatrix = setInvMatrix, getInvMatrix = getInvMatrix)
       
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
       inv_Matrix <- x$getInvMatrix()
       
       if(!is.null(inv_Matrix)){
              print("This is cached data")
              return(inv_Matrix)
       }
       
       matrix <- x$get()
       inv_Matrix <- solve(matrix)
       x$setInvMatrix(inv_Matrix)
       inv_Matrix
}
