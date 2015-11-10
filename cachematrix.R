## The below functions create a special matrix that has in-built functions to set and get matrices
## and their inverse. The cachesolve function uses the in-built functions to reduce computation 
## time by using the already-computed inverse of the matrix.

## This functions creates an empty matrix that contains a list of 4 funtions to set/get the matrix 
## and set/get the inverse of the matrix. Unless the optional argument 'inverse' is explicitly set 
## to FALSE, the function automatically calculates the inverse of the matrix when a matrix is set. 

makeCacheMatrix <- function(x = matrix(), inverse = TRUE) {
    i <- NULL
    set <- function(y) {
        x <<- y
        if(inverse){
            i <<- solve(y)
        } else {
            i <<- NULL
            inverse <- TRUE
        }
    }
    get <- function() x
    setinvs <- function(invs) i <<- invs
    getinvs <- function() i
    list(set=set, get=get, setinvs=setinvs, getinvs=getinvs)
}


## cacheSolve gets the inverse of the matrix that has already been calculated. If the inverse
## hasn't been computed, the functions does the calculations and sets the inverse for the matrix.
## This function also checks whether there was any data in the matrix supplied by the user, and 
## returns a message. In this case, the function does not setinvs for the matrix.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinvs()
    if(!is.null(i)){
        message("Getting cached data...")
        return(i)
    } 
    
    mat_data <- x$get()
    if(!is.na(mat_data)){
        i <- solve(mat_data, ...)
        x$setinvs(i)
        return (i)
    } else {
        message("There is no data in matrix supplied!")
    }
}

## ~~NAVNEETH K~~
