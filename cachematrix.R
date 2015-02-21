## makeCacheMatrix creates a special "vector", which is a list containing a function to 
## 1.set the value of matrix x
## 2.get the value of matrix x
## 3.set the value of the inverse of x
## 4.get the value of the inverse of x

makeCacheMatrix <- function(x = matrix()) { 
        m <- NULL
        set <- function(y) {  #set the value of the vector
                x <<- y
                m <<- NULL
        }
        get <- function() x  #get the value of the vector
        setinverse <- function(solve) m <<- solve # set the value of the inverse of the matrix
        getinverse <- function() m  # get an object name  # get the value of the inverse of the matrix
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)  # creates a list with functions
}

## The following function calculates the inverse of the special "matrix" created with 
## the above function. However, it first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation.
## Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache 
## via the setinverse function.

cacheSolve <- function(x, ...) { # to computes the inverse of a matrix 
        m <- x$getinverse()	#get the value of the inverse of the matrix from the makeCacheMatrix
        if(!is.null(m)) {	#if inverse is has been localized
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...) # computes the inverse of a matrix x
        x$setinverse(m) 	#sets the inverse of a matrix x in the cahe
        m		# print result 
}
