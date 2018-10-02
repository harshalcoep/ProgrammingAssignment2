## Program to efficiently retrieve the inverse of a matrix


## makeCacheMatrix function takes matrix x as input and creates an object with four methods
## set() creates a matrix and stores it in cache
## get() return the matrix stored in cache
## setInv() stores the inverse of the matrix in cache
## getInv() returns the inverse of the matrix stored in cache

makeCacheMatrix <- function(x = matrix()) {
  xinv <- NULL
  set <- function(y){
      x <<- y
      xinv <<- NULL
  }
  get <- function() x
  setInv <- function(inv) xinv <<- inv
  getInv <- function() xinv
  list(set = set, 
       get=get, 
       setInv=setInv, 
       getInv=getInv)
}


## cache Solve function computes the inverse of a matrix x (if not already available in cache)
#if the inverse is already available in cache then computation is not performed again

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getInv()
    if(!is.null(inv)){
        message("inverse of the matrix is already cached, getting cached data")
        return(inv)
    }
    m <- x$get()
    inv <- solve(m)
    x$setInv(inv)
    inv
}
