## this 2 functions are used to calculate the inverse of a matrix. 
##in order to save time the seconed function first checks if the 
##inverse was already calculated. In case the inverse was calculates 
##the function mentions it and present the inverse that was previously calculated.
##if the inverse wasnt calculated the function calculates it and prints the value.



##this first function takes the input matrix and outputs a list containing the 
##matrix and a place for the matrix's inverse (m). if the inverse was not yet calclated
##this place remains NULL.
makeCacheMatrix <- function(x = matrix()) {
       m <- NULL                 ##intiate the matrix inverse place to be NULL.
       
     set <- function(y) {        ##sets x to be the matrix and m to be the emty 
       x <<- y                   ##place for the inverse - in the outer environment  
       m <<- NULL
  }
     get <- function(){x}        ##presents the matrix
  
     setinverse <- function(inverse) {  ##this function takes a value as an input  
       m <<- inverse                    ##and assign it to m in the outer 
     }                                  ##environment-the inverse matrix 
  
     getinverse <- function() {m}   ##presents the inverse matrix
  
     list(set = set, get = get,     ##this is the list which is the whole
          setinverse = setinverse,  ## function's output
         getinverse = getinverse)
 
}


## this functions takes the list from the privious function as an input, checks
##if the inverse was already calculated- if it was the function presents a 
##messeage and the previously calculated inverse matrix, if it wasnt the 
##function calculate the inverse and presents it.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()           ##assign the inverse value from the input to m
  
     if(!is.null(m)) {     ##checks if its a value-if it was already calculated
         message("getting cached data")   ##if it was-- presents a messeag
         return(m)                        ##and returns the inverse matrix
     }
     data <- x$get()       ##if it wasnt it assigns the matrix to data
  
     m <- solve(data, ...) ## calculates the inverse using "solve" function
  
     x$setinverse(m)       ##sets the inverse value in the input list to solution 
  
     m                     ##returns the inverse matrix
 }
