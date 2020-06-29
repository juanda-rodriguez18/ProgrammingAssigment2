## This function is separated in two parts:

##      First we stablish makeCacheMatrix, where we define 
##    different names and aspects that i will explain later.

##      And next, we stablish the cacheSolve, where we´re defining
##    what we want to show, and the names of those results in order
##    to understand the code. Also i will explain this part later.

##  The principal mission of this code is to figure out the inverse
## of any matrix, according to math rules, using the R code structure.
## ___________________________________________________________________
 

##  First we stablish that x will be the matrix, and in there we put  
## inv that is NULL for now, and set, that is another function to set 
## the value of the initial function. Next, get is to show own matrix,
## setInverse is to set the value of the inverse, and getInverse to 
## show it. 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() {x}
  setInverse <- function(inverse) {inv <<- inverse}
  getInverse <- function() {inv}
  list(set= set, get= get, setInverse = setInverse, getInverse = getInverse)
}


##  This part is to calculate the inverse itself, we start defining her
## value  that is getInverse. We put a message if this value is NULL,
## and the process continues with the new value of inv, the solution 
## of it, which is include get, and later we stablish the set of this
## inverse value and the inv itself. (Thank you and good luck!!)

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}
