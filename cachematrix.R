## Put comments here that give an overall description of what your
## functions do

## This function creates a cache matrix, which allows you to calculate the inverse of a matrix
## without having to compute it repeatedly. It does so by doing the following:
## 1. Sets the value of the matrix
## 2. Gets the value of the matrix
## 3. Sets the value of the inverse
## 4. Gets the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
  set <- function(y) {
          x <<- y
          i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## This function then computes the inverse of the special matrix. If the inverse has 
## already been calculated, it simply just retrieves the inverse.

cacheSolve <- function(x, ...) {
   i <- x$getinverse()
  if (!is.null(i)) {
          message("getting cached data")
          return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}

## Let's test this function, shall we?

## s <- rbind(c(2, 4), c(6, 8))
## n <- makeCacheMatrix(s)
## n$get()
##       [,1] [,2]
## [1,]    2    4
## [2,]    6    8
## cacheSolve(n)
##       [,1]  [,2]
## [1,] -1.00  0.50
## [2,]  0.75 -0.25

## It is a success.
