## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
      
      inv <- NULL                             ##Set a variable for Inverse matrix as empty 
      set <- function(y) {                    ##Function that set the matrix to cache
            x <<- y                      
            inv <<- NULL                      ##Set null value of inverse matrix to cache 
      }
      get <- function() x                     ##Function that returns the matrix from cache
      setinv <- function(solve) inv <<- solve ##Function that set the x inverse matrix to cache 
      getinv <- function() inv                ##Function that get the x inverse matrix fromn cache
      list(set = set, get = get,              ##Final result - function returns...
           setinv = setinv,                   ##...the list of 4 above functions          
           getinv = getinv)                   
      
}                                             


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      s <- x$getinv()                         ##assign the x matrix's cache value to s
      if(!is.null(s)) {                       ##if cache value is not null
            message("getting cached data")
            return(s)                         ##return the cache value, no computation was done
      }
      data <- x$get()                         ##if cache value is empty
      s <- solve(data, ...)                   ##we compute the Inverse matrix
      x$setinv(s)                             ##set the result to cache
      s                                       ##return the computed inverse matrix
}
