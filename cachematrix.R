## R Programming (rprog-009) Assignment 2

## This function creates a special "matrix" object that can cache its inverse. It is similar to the provided function makeVector which returns a vector.
makeCacheMatrix <- function(x = matrix()) {
    		# sets x equal to an empty matrix
        inv <- NULL
        # Initialize the inverse equal to NULL. inv will be our 'inverse' and it's reset to NULL every time makeCacheMatrix is called
        set <- function(y){
                x <<- y
                # set function assigns the argument to x
                inv <<- NULL
                # Once the set function is called, inv will be re-initialized to NULL
        }
        get <- function() x
        # get function returns the matrix
        setinv <- function(inverse) inv <<- inverse
        # setinv rewrites the value of inv passes as an argument to inverse
        getinv <- function() inv
        # getinv returns the inverse
        list(set = set, get = get, setinv = setinv, getinv = getinv)
        # list will create a list of the functions
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.  It is similar ro the provided function cachemean which computes the mean.
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Function will return a matrix that is the inverse of 'x'
		    inv <- x$getinv()
        # Retrieves the value of the inverse
        if(!is.null(inv)){
                message("Cache hit...Retrieving data from the cache!!!")
                return(inv)
                # If the value of inv is NOT null then cacheSolve returns that value and displays a message
        }
        # If the value of inv is NULL, then retrive matrix x and calculate the inverse with the solve() function
        message("No cache hit. Reading and calculating the inverse...")
        d <- x$get()
        inv <- solve(d, ...)
        x$setinv(inv)
        # set inv to the new value   
        inv #Return the value of inv
}