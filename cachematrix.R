## Put comments here that give an overall description of what your
## functions do

## Initialising this function to a variable creates an object containing
## x and inversMatrix, where x is a matrix given to the function,
## and inversMatrix is initially set to "NULL".

## The output of the function is a list of functions, which can be accesses
## over the Listoperator of the newly created object.
## "set" takes a matrix as the input and saves it in x, as well as sets 
## the variable "inversMatrix" to "NULL".
## "get" takes no input and returns the saved matrix in variable "x".
## "setInverse" takes an already calculated inverse of the matrix in "x"
## and saves to to the variable "inversMatrix". This inverse is calculated
## within the second function "cacheSolve".
## "getinverse" returns the value in "inversMatrix", if something is saved
## there, otherwise returns "NULL".

makeCacheMatrix <- function(x = matrix()) {
        inversMatrix <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
        
}


## We give our object created with "makeCacheMatrix" to this second
## function "cacheSolve". "cacheSolve" will access some functions of
## "makeCacheMatrix" to check if an inverse is already saved in the 
## object, if so, it will return this value with a message.
## If no object is saved in the "inversMatrix" variable in our object,
## it will take the matrix saved in "x", calculate the inverse and save it 
## in "inv", then use the "setInverse" function in "makeCacheMatrix" to save 
## the newly solved matrix to the "makeCacheMatrix" object.
## "x" is out "makeCacheMatrix" object given to the function.
## "x$getInverse" takes the value in "inversMatrix" from the "x" and 
## saves it in "inv".
## The conditional checks if the value is NULL.
## inv != NULL => message output and return of the matrix in "inv"
## inv == NULL:
## => the Matrix from the object "x" is saved in "mat".
## the inverse of that Matrix is calculated ("solve") and saved in "inv".
## The "setInverse" function within "x" is accessed and given the inverse
## to save it in the  object "x".
## And the inverse is returned by the "cacheSolve" function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}
