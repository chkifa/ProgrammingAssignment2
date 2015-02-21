## The first function, makeCacheMatrix, creates a special "matrix" object that 
#can cache its inverse. return a list of:

#1-functions to set the matrix
#2-functions to get the matrix
#3-functions to set the inverse of matrix
#4-functions to get the inverse of matrix

#this list is used as the input to second function : cacheSolve()

#The second function cacheSolve, computes the inverse of the special "matrix" returned 
#by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has 
#not changed), then the cacheSolve should retrieve the inverse from the cache.
#Otherwise, it calculates the inverse of the data and sets the value of the inverse in 
#the cache via the setinverse function.


##makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    #initialize cached value to NULL
    inv <- NULL
    # create the matrix in the working environment
    set <- function(y=matrix()) {
        x <<- y
        inv <<- NULL
    }
    # get the value of the matrix
    get <- function() x
    # invert the matrix and store-it in cache
    setinverse <- function(inverse) inv <<- inverse
    # get the inverted matrix from cache to use it in cacheSolve function
    getinverse <- function() inv
    
    # return a list of functions created, to the working environment
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## cacheSolve: This function computes the inverse of the special "matrix" 
##returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    # if the inverse has already been calculated
    if(!is.null(m)) {
        # get it from the cache and skips the computation.
        message("getting cached data")
        return(m)
    }
    # otherwise, calculates the inverse
    data <- x$get()
    m <- solve(data, ...)
    # sets the value of the inverse in the cache via the setinverse function.
    x$setinverse(m)
    m
}
####example
#> a0<-rbind(c(1,0,0),c(0,1,0),c(0,0,1))
#> a1<-makeCacheMatrix(a0)
#> a1$get()
#[,1] [,2] [,3]
#[1,]    1    0    0
#[2,]    0    1    0
#[3,]    0    0    1
#> a1$getinverse()
#NULL
#> cacheSolve(a1)
#[,1] [,2] [,3]
#[1,]    1    0    0
#[2,]    0    1    0
#[3,]    0    0    1
#> cacheSolve(a1)
#getting cached data
#[,1] [,2] [,3]
#[1,]    1    0    0
#[2,]    0    1    0
#[3,]    0    0    1
#> a1$getinverse()
#[,1] [,2] [,3]
#[1,]    1    0    0
#[2,]    0    1    0
#[3,]    0    0    1
