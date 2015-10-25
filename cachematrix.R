# This function creates a special matrix object that can cache its inverse.
# It comprises of a list of functions namely
   # set()- to set the matrix
   # get()- to get the matrix
   # setinversematrix()- set the value of inverse of the matrix
   # getinversematrix()- get the value of inverse of the matrix
   


## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
 inve<- NULL
    set<- function(y) {
      x<<- y
     inve<<- NULL
     }
  get<- function() x
   setinversematrix<- function(inverse) inve <<- inverse
   getinversematrix<- function() inve
   list(set = set, get = get,
        setinversematrix = setinversematrix,
        getinversematrix = getinversematrix)
}


#cacheSolve function calculates the inverse of the matrix object which is set using the makeCacheMatrix function. 
# this function firsts check if the inverse has already been calculated or not by checking using the getinversematrix function.
# if the value is already calculated it returns the message "Cached Data Found." with the inverse matrix value.
# otherwise it calculates the inverse of the matrix and sets it value in the cache via the setinversematrix function.

cacheSolve <- function(x, ...) {
       inve <- x$getinversematrix()
  if(!is.null(inve)) {
    message("Catched Data found.")
    return(inve)
  }
  data <- x$get()
  inve <- solve(data)
  x$setinversematrix(inve)
  inve
}
