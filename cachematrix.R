#Computes the inverse of a matrix to remove the need to constantly calculate this operation.

#Sets and gets the value of the matrix and then sets and gets the value of the inverse.

makecachematrix <- function(x = Matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) {inv <<- inverse}
        getinverse <- function() (inv)
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

# calculates the inverse of the matrix from the above function. If this calculation has already been done, it retrieves the answer
cachesolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
