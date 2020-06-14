## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

### This function creates a special "matrix" object that can cache its inverse.
### Modify makevector code example for assignment

makeCacheMatrix <- function(x = matrix()) {   # Note: argument default mode is changed to "matrix"
    inv <- NULL                               # initialise inv as null to hold value of matrix inverse
    set <- function(y) {                      
      x <<- y
      inv <<- NULL
    }
    get <- function() x                       # Retrieve x from parent environment of makeCacheMatrix(); from argument
    
    setinverse <- function(inverse) inv <<- inverse # Repeat steps above to set and get inverse value
    getinverse <- function() inv                    
    list(set = set, get = get,                      # Gives the names to the functions defined above; to allow the use of $
         setinverse = setinverse,
         getinverse = getinverse)
}


## Write a short comment describing this function
### This function computes the inverse of the special "matrix" returned by `makeCacheMatrix` above. 
### If the inverse has already been calculated (and the matrix has not changed), then
### `cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()                           # Retrieve mean from object passed into function; 
                                                    # note: object should be from makeCacheMatrix
    if(!is.null(inv)) {                             # checks if inv is not equal to null; if true, the inv value is returned
      message("getting cached data")
      return(inv)
    }
    data <- x$get()                                 # if inv is equal to null, cacheSolve gets the vector from the input object,
    inv <- solve(data, ...)                         # and solve for the inverse matrix
    x$setinverse(inv)                               # and uses the setinverse function to set the inverse matrix in the input object
    inv                                             # and returns and prints the value of the inverse matrix
  }


#test example (extra for testing if functions above work)
test <- matrix(c(1.24,1.45,6.5,2.4,5.5,7.5,8.1,2.3,4.8),3,3)
solve(test)

mymatrixA <- makeCacheMatrix(test)
cacheSolve(mymatrixA)
