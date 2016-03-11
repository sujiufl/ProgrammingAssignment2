## R Programming: Programming Assigment2: Lexical Scoping
## Writing a R function to able to cache potentially time-consuming computation

## Function :- 'makeCacheMatrix' creates a special "matrix" object that can 
## cache its inverse.
## What it does:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix 
## 4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Function :- 'cacheSolve' computes the inverse of the special "matrix" returned
## by 'makeCacheMatrix'. If the inverse has already been calculated (and the matrix has not changed)
## then the function will retrieve the inverse from the cache
## What it does:
## 1. checks to see if the inverse of the matrix has already been computed
## 2. if so, it gets the inverse of the matrix from the cache and skips computation
## 3. otherwise, it calculates the inverse of the matrix using the 'solve' function  
## 4. set the value of the inverse of the matrix in the cache via the 'setinverse' function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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
