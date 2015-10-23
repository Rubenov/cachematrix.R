## The function, which is part of a group compounded by this one and the function "cacheSolve", creates a special object made of four functions that can cache and return its input value and its inverse.

makeCacheMatrix <- function(x = matrix()) {
    
    m <- NULL
    
    set <- function(y) { #Set the value of a matrix (It's the only function which is not called from the "cacheSolve" function and it's used to introduce a new matrix value, with the purpose of set the "m" parameter the NULL value so that "cacheSolve" function could calculate the inverse matrix again.)
        x <<- y
        m <<- NULL
    }
    get <- function() x # A function that returns the value input "x".
    setinv <- function(inv) m <<- inv # A function to cache the inverse.
    getinv <- function() m # A function that returns the invert matrix cached value.
    list(set=set, get = get, setinv=setinv, getinv = getinv) #A list of functions which is also the imput argument of the "cacheSolve" function.
}


## This function returns the inverse matrix of the previous function input called 'x'. If the inverse has already been calculated and the matrix has not been set again, the function looks up the inverse value in the cache, which is returned instead of repeating the calculus.

cacheSolve <- function(x, ...) {
    
    m <- x$getinv() #The "m" parameter is here updated, either with the "NULL" value if it's the first time the inverse is being calculated or with the inverse value if it had been calculated before.
    
    if(!is.null(m))  { #In case the "m" parameter gets the inverse value, then a message is shown saying the inverse value is going to be recovered from cached data.
        
        message("getting cached data")
        return(m)
    }
    
    y <- x$get() #Use the "get" function to bring back the matrix value.
    m <- solve(y, ...) #The "solve" function calculates the inverse of the matrix and sets it in "m".
    x$setinv(m) #"setinv" function gets the inverse value for its storage.
    m #Return the value of the invert matrix.    
}