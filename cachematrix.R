## In this case I will follow the steps made at the example given.
## That's why it will look a lot to the example.
## First we will set the value of the matrix.
## Then we will get the value of that matrix.
## After that, we want to set the inverse of this matrix.
## Finally, we will get the value of that inverse. 
## Note that X and Y were put in Caps since it is the conventional representation
## of a matrix.

makeCacheMatrix <- function(X = matrix()) { 
  I <-   NULL                    
  set <- function(Y){
    X <<- Y
    I <<- NULL
  }
  get <- function() X
  setinverse <- function(inverse) I <<- inverse
  getinverse <- function() I
  list(set = set, get = get,       ##naming set and get refer to set() and 
                                   ##get() functions
       setinverse = setinverse,    ##naming setinverse to setinverse() function
       getinverse = getinverse)    ##naming getinverse to getinverse() function
}

## Now, this next function will check if there is already a calculated inverse
## matrix in the cache or if we should calculate it.

cacheSolve <- function(X, ...){
  I <- X$getinverse()
  if(!is.null(I)){
    message("getting cached data")     ##Informs me that this matrix was already
                                       ## cached.
    return(I)
  }
  data <- X$get()                      ##Solves the matrix if not in cache.
  I <- solve(data, ...)
  X$setinverse(I)
  I
}
