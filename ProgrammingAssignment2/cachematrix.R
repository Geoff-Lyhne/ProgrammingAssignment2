
## This function creates a special "matrix" list that can cache its inverse.
makeCacheMatrix <- 
        function(x = matrix()) {
                inv <- NULL             # Will hold the inverted value
                set <- function(y) {    
                        x <<- y         # Sets the matrix value to y and changes 
                        inv <<- NULL    # inv back to NULL do clear previous use.
                }                       
                get <- function() x     # Creates get with the value of the matrix
                setinverse <- function(inverse) inv <<- inverse # Assigns inv in parent environment
                getinverse <- function() inv    # Gets the value of inv
                list(set = set, get = get,      # Creates a list with all information
                     setinverse = setinverse,
                     getinverse = getinverse)
        }



## cacheSolve calculates the inverse of the special "table" created 
## with makeCacheMatrix using 'solve()'. However, it first checks to see if the 
## inverse has already been calculated. If so, it gets the inverse from the 
## cache and skips the computation. Otherwise, it calculates the inverse of the 
## table and sets the value of the table in the cache via the setinverse function.

cacheSolve <- 
        function(x, ...) {
                inv <- x$getinverse()   # Assigns inverse from makeCacheMatrix
                if(!is.null(inv)) {
                        message("Getting cached data")  # Pulls previously saved data
                        paste(inv)                      # only if it hasn't been saved.
                }
                matrix <- x$get()               # Pulls matrix from makeCacheMatrix
                inv <- solve(matrix, ...)       # Inputs necessary matrix
                x$setinverse(inv)               # Sets the inverse of x
                inv                             # Displays inverse
        }

