## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {            ##Defines the function as well as the arguments to be passed
    inv <- NULL                                        ##Intializes the final result variable to NULL
    set <- function(y) {                               ##Defines the setter, the global argument will be given to y
      x <<- y                                          ##The values are then stores in a parent environment variable X which is different from the Global Environment variable X
      inv <<- NULL                                     ##The inv variable is again set to NULL. This is done so that every time a new matrix argument is passed and setinverse is called, the old value os inv is not returned
    }
    get <- function() x                                ##This is merelt to return the value stored in the Parent Environemtn Variable X
    setinverse <- function(inverse) inv <<- inverse    ##This stores the value of cachesolve's inv in makecachematrix's inverse which is then saved in makecachematrix's inv
    getinverse <- function() inv                       ##This is merelt used to return the value of inv
    list(set = set, get = get,                         ##The fllowing 3 lines of code create a named list of the above 4 functions
         setinverse = setinverse,
         getinverse = getinverse)
  }


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()                              ##The inv value storedin the above function is saved in this inv variable
      if(!is.null(inv)) {                              ##This checks if the inv variable is NULL or not
            message("getting cached data")             ##If it is not NULL theis message is displayed
        return(inv)                                    ##Since the inv is not NULL, the saved value is returned                                 
  }
    data <- x$get()                                    ##If inv is NULL, the get function is executed and the data is sored in a new variable "data"
    inv <- solve(data, ...)                            ##The solve function is used to calculated the inverse of "data" and stored in "inv"
    x$setinverse(inv)                                  ##This variable "inv" is then passed as an argument to the setinverse function,
    inv                                                            ##When the above happens, the data of the above inv variable gets stored in Part1's "inverse"
}                                                                  ##variable when then gets stored in Part1's inv variable   


b = makeCacheMatrix(c(5,6,7,8), nrow=2, ncol=2)        ##This is the wring way to do
b
cacheSolve(b)

c = matrix(rnorm(4), nrow = 2, ncol = 2)               ##This is also wrong
makeCacheMatrix(c)
cacheSolve(c)
e = makeCacheMatrix(matrix(rnorm(4), 2, 2))           ##This is correct

f=cacheSolve(e)
f


