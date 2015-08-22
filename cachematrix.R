#Matrix inversion is usually a costly computation therefore the purpose
#of the following pair of functions is to cache the inverse of a matrix.
#This deomonstrates the use of lexical scoping and utilizes the << assignment
#operator to assign a value to an object in a different environment to the current.

#1. makeCacheMatrix
#Creates a special "matrix" object that can cache it's inverse.
#It contains four functions within it to set and get the value of the matrix and 
#to set and get the value of the inverse of the matrix.
#It is used in conjunction with the cacheSolve function which will either
#return the cached inverse or calculate it.

makeCacheMatrix <- function(x = matrix()) {
    
    inv_value <- NULL                                       #Set default value of NULL
    set <- function(y) {                                    #Set the matrix
        x <<- y                                             #This will assign the value to the global environment
        inv_value <<- NULL                                  #Set default value of NULL
    }
    get <- function() x                                     #Get the value of the matrix
    setinverse <- function(inverse) inv_value <<- inverse   #Set the value of the inverse of the matrix
    getinverse <- function() inv_value                      #Get the value of the inverse of the matrix
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)                           #A list of the functions contained within the function
}

#2. cacheSolve
#Returns a matrix that is the inverse of 'x' provided to the makeCacheMatrix function.
#Checks if the inverse of the matrix has been cached and if so returns the value.
#If it isn't cached it calculates the value of the inverse of the matrix.
#Function assumes a square invertible matrix.

cacheSolve <- function(x, ...) {

    inv_value <- x$getinverse()                             #Get the cache from the makeCacheMatrix function
    if(!is.null(inv_value)) {                               #Check if the cache exists (ie not NULL)
        message("getting cached data")                      #If it does, then display message that this is what will be returned
        return(inv_value)                                   #Return the cache and end function
    }
    data <- x$get()                                         #Cache didn't exist so we get the matrix
    inv_value <- solve(data, ...)                           #Calculate the value of the inverse of the matrix
    x$setinverse(inv_value)                                 #Cache the value of the inverse of the matrix using setInverse function 
                                                            #from within the makeCacheMatrix function
    inv_value                                               #Return the value of the inverse of the matrix
}
