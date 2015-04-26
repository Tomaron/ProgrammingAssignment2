## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) { ##makeCacheMatrix stores list of functions
        m <- NULL ## m variable is assigned NULL
        set <- function(y) { ## function changes the stored vector in main function
                x <<- y ##substitutes vector x with y in the main function
                m <<- NULL ##updates the NULL value of m
        }
        get <- function() x ##returns vector x stored in the main function
        setsolve <- function(solve) m <<- solve ##setsolve stores the value of the input in m variable
        getsolve <- function() m ##getsolve returns the stored value in m variable
        list(set = set, get = get, 
             setsolve = setsolve,
             getsolve = getsolve) ## list stores all functions in function makeVector
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {    ## Verifies and return the matrix that is the inverse of 'x'
	            m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
               inv <- cacheSolve(x) ## gets and assign the cached data to inv
               inv   ## returns the inverse matrix value of x
}
