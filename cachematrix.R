# makeCacheMatrix function creates a special "matrix" object that can cache its inverse.


# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix


makeCacheMatrix <- function (x = matrix()){
    # We cache the inverse of the matrix "x" in the variable "inverse".
    # If there's no matrix, it will be "NULL".
    inverse <- NULL
    
    # The set function makes to set a specific matrix to the instance of this class of "matrix".
    set <- function(y){
     x<<- y
      inverse <<- NULL
     }
    
    # Get the value of the matrix.
    get <- function()x
      
    # Set the value of inverse of the matrix.
    setinverse <- function(i){
      inverse <<- i
     } 
 
    # Get the value of inverse of the matrix.
    getinverse <- function(){
      inverse
     }
    
    # "makeCacheMatrix" is a list.
    list(set= set, get=get,
       setinverse =setinverse,
       getinverse =getinverse)
}

# This function will compute the inverse of te special "matrix" returned by makeCacheMatrix above.
# If the inverse has already been calculated, the "cacheSolve" should retrieve the inverse from cache.

cacheSolve <- function(x, ...){
    
    # Reveive the parameter "x" as an instance of "makeCacheMatrix".
    # We first check the value of the inverse.
    inverse <- x$getinverse()
    if(!is.null(inverse)){
        message("getting cached data")   
    }
    else{
    data <-x$get()
    inverse<- x$setinverse(solve(data));
    }
     inverse
}

## Example run:
## > x=rbind(c(2,3), c(3,2))
## >  m= makeCacheMatrix(x)
## > m$get()
## [,1] [,2]
## [1,]    2    3
## [2,]    3    2
## > cacheSolve(m)
## [,1] [,2]
## [1,] -0.4  0.6
## [2,]  0.6 -0.4
