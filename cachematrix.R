## Complex operations like matrix invertion can take a long time to compute. 
## If we use this operations several time without changing the base data
## it make sense to use the result from the first computation.


## The following code creates two functions that work together to accomplish
## exactly that:


## The two functions are makeCacheMatrix and cacheSolve. The first function creates
## a sepecial type of matrix that has a list and have some public methods.
## The second function use the special matrix list created with the 
## first function and avoids repeating the computation if data is same
## and the inverse matrix is already calculated.




## makeCacheMatrix creates a list with a special type of matrix 
## with public methods.
## This methods are: get (matrix), set (matrix), get(inverse matrix), 
## set(inverse matrix).


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) { ## initialize a matrix
    x <<- y ## set the matrix for a variable in the
    ## parent environment (calling function)
    
    m <<- NULL ## set variable m in the parent environment
    ## (calling function) to null
  }
  get <- function() x ## just gets the matrix
  
  setinverse <- function(inverse) m <<- inverse ## set the inverse 
  ## matrix
  
  getinverse <- function() m ## Get the inverse matrix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse) ## create the public methods
  
}




## This function creates a inverse matrix from the special type of matrix
## created with makeCacheMatrix, if that's not already calculated.
## If the inverse matix is already calculated, then gets the previous
## calculation


cacheSolve <- function(x, ...) { ## argument is a makeCacheMatrix object
  
  m <- x$getinverse() ## get the inverse matrix from makeCacheMatrix object
  
  if(!is.null(m)) { ## if inverse matrix is not null just return
                    ## the previous calculated value
    message("getting cached data")
    return(m)
  }
  data <- x$get() ## get the original matrix
  
  m <- solve(data, ...) ## invert the matrix
  x$setinverse(m) ## set object makeCacheMatrix with calculated
                  ## invert matrix for future use
  m
}
