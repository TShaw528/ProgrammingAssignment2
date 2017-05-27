#Coursera Programming Project 2
#2 Functions
#makeCatcheMAtrix generates a special "matrix" object that can cahe its inverse
#cacheSolve computes the inverse of the special matrix

#This function sets the value of a matrix, retrieves it, finds the inverse value then retrieves that

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)

}

#This function computes the inverse as was retrieved above

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
