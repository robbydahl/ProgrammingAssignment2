## Put comments here that give an overall description of what your functions do

# [cacheSolve function calculates the inverse of a matrix that is defined in the function makeCacheMatrix
#by fetching it through get() function. This inverse is then set in the function setInverse()
#so anytime same inverse is required cacheSolve gets the resultant through the getInverse() function
# in the cache of the program.]

## Write a short comment describing this function
#[ This function creates a vector containing a function to set and get the value of the vector
# set and get the value of the inverse.]

makeCacheMatrix <- function(x = matrix()) {
     inv<-NULL
     set<- function(y){
       x<<- y
       inv<<- NULL
     }
     get<-function()x
     setInv <- function(inverse) inv<<- inverse
     getInv <- function() inv
     list(set = set, get= get, setInv = setInv, getInv = getInv)
}


## Write a short comment describing this function
#[This following function checks to see if the inverse is calculated, if calculated gives the inverse from the cache, else 
# calculates the inverse and then sets the value of inverse..]

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv<-x$getInv()
  if(!is.null(inv)){
    message("getting the cached data")
    return(inv)
  }
    matrix<- x$get()
    inv<- solve(matrix,...)
    x$setInv(inv)
    inv
}
