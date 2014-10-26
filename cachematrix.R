## The purpose of this code is to cache an inverse of matrix
## 'makeCacheMatrix' offers methods to set and get matrix inverse. While
## cacleSolve computes the matrix inverse in case it is not already availble
## in the cache

## makeCacheMatrix allows set & get of the matrix, while inc_Matrix has the
## inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
       
       inv_Matrix <- matrix()
       
       #set and get matrix
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


## cacheSolve retrieves inverse matrix from the cache, and in case not
## found, computes and stores it for future use

cacheSolve <- function(x, ...) {
       
       ## Return a matrix that is the inverse of 'x'
       inv_Matrix <- x$getInvMatrix()
       
       #check if inverse matrix is available from cache
       if(!is.null(inv_Matrix)){
              print("This is cached data")
              return(inv_Matrix)
       }
       
       # Inverse matrix is not available, hence compute and cache it
       matrix <- x$get()
       inv_Matrix <- solve(matrix)
       x$setInvMatrix(inv_Matrix)
       inv_Matrix
}
