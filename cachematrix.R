## Working with a matrix in a way that permits get advantage of
## previous calculations of their inverse maintainig a cache structure.

## makeCacheMatrix() constructs the matrix structure with cache values to speed
## eventual repeated calculations of the inverse.
## The structure is a list of four functions for getting and setting both
## the matrix and their inverse matrix.
makeCacheMatrix <- function(x = matrix()) {
        lastMatrixInverted <- NULL #initializing last input matrix inverted
        inverse <- NULL            #initializing last result of inversion
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inputM,inverseM) {
                inverse <<- inverseM           #last result of inversion
                lastMatrixInverted <<- inputM  #last input matrix
        }
        getinverse <- function() inverse
        getLastInverted <- function() lastMatrixInverted
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse,
             getLastInverted = getLastInverted)
}


## cacheSolve() calculates de inverse of an special matrix structure
## constructed by makeCacheMatrix() function.
## This function takes advantage from makeCacheMatrix() one, 
## calculating the inverse only with is not calculated yet by previous calls.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getinverse()
        data <- x$get()
        lastMatrixInverted <- x$getLastInverted()
        if(!is.null(inverse)) {
                if(identical(data,lastMatrixInverted)) {
                        message("getting cached data")
                        return(inverse)
                }
        }
        inverse <- solve(data, ...)
        x$setinverse(data,inverse)  #storing input and output matrices
        inverse
}


#__________________________________________
#
# Example of invocation and console output
#__________________________________________
# > source("cachematrix.R")
# > cacheMatrix <-makeCacheMatrix(matrix(c(1,2,3,4),nrow=2,ncol=2))
# > inverseMatrix<-cacheSolve(cacheMatrix)
# > print(inverseMatrix)
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# > inverseMatrix<-cacheSolve(cacheMatrix)
# getting cached data
# > print(inverseMatrix)
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# > cacheMatrix$get()
# [,1] [,2]
# [1,]    1    3
# [2,]    2    4
# > cacheMatrix$set(matrix(c(4,3,1,1),nrow=2,ncol=2)) #changing matrix!!!
# > inverseMatrix<-cacheSolve(cacheMatrix)
# > print(inverseMatrix)
# [,1] [,2]
# [1,]    1   -1
# [2,]   -3    4
# > inverseMatrix<-cacheSolve(cacheMatrix)
# getting cached data
# > print(inverseMatrix)
# [,1] [,2]
# [1,]    1   -1
# [2,]   -3    4
# > 