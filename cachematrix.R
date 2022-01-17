## Put comments here that give an overall description of what your
## functions do

## Initialising this function to a variable creates an object containing
## matx and inversMatrix, where matx is a matrix given to the function,
## and inversMatrix is initially set to "NULL".

## The output of the function is a list of functions, which can be accesses
## over the Listoperator of the newly created object.
## "set" takes a matrix as the input and saves it in matx, as well as sets 
## the variable "inversMatrix" to "NULL", saving the variables in the
## environment of the created object by "<<-".
## "get" takes no input and returns the saved matrix in variable "matx".
## "setInverse" takes an already calculated inverse of the matrix in "matx"
## and saves to to the variable "inversMatrix". This inverse is calculated
## within the second function "cacheSolve" and is saved in the higher order
## environment of the function by using "<<-".
## "getinverse" returns the value in "inversMatrix", if something is saved
## there, otherwise returns "NULL". 

makeCacheMatrix <- function(matx = matrix()) {
        inversMatrix <- NULL
        set <- function(y) {
                matx <<- y
                invers <<- NULL
        }
        get <- function() matx
        setInverse <- function(inverse) invers <<- inverse
        getInverse <- function() invers
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
## it will take the matrix saved in "obj", calculate the inverse and save it 
## in "invers", then use the "setInverse" function in "makeCacheMatrix" to save 
## the newly solved matrix to the "makeCacheMatrix" object.
## "obj" is out "makeCacheMatrix" object given to the function.
## "obj$getInverse" takes the value in "inversMatrix" from the "obj" and 
## saves it in "invers".
## The conditional checks if the value is NULL.
## invers != NULL => message output and return of the matrix in "invers"
## invers == NULL:
## => the Matrix from the object "obj" is saved in "matritze".
## the inverse of that Matrix is calculated ("solve") and saved in "invers".
## The "setInverse" function within "obj" is accessed and given the inverse
## to save it in the  object "obj".
## And the inverse is returned by the "cacheSolve" function. x

cacheSolve <- function(obj, ...) {
        ## Return a matrix that is the inverse of 'obj'
        invers <- obj$getInverse()
        if (!is.null(invers)) {
                message("getting cached data")
                return(invers) 
        }
        matritze <- obj$get()
        invers <- solve(matritze, ...)
        obj$setInverse(invers)
        invers
}
