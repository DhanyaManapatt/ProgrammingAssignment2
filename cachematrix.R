## Coursera R Programming Assignment
## Function - makeCacheMatrix

## This function creates a special Matrix that can cache its inverse
## The function returns a list containing the function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse matrix
## 4. get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  ## Assign NULL value to the invs matrix
  invs <- NULL
  ## Set the matrix
  set <- function(y) {
    ## Assign new value
        x <<- y
    invs <<- NULL
  }
  ## get the matrix and then set and get the inverse matrix
  get = function() x
  setinvs = function(inverse) invs <<- inverse 
  getinvs = function() invs
  ## return the list of functions
    list(set=set, 
    get=get, 
    setinvs=setinvs, 
    getinvs=getinvs)
}

## Function - cacheSolve
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix function above.
## If the inverse has already been calculated (and the matrix has not changed)
## then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
   invs = x$getinvs()
     # Check if inverse has already been in cache ie. cache is not null
     if (!is.null(invs)){
    message("getting cached data")
    return(invs)
  }
    # calculate the inverse if it is not in cache 
  matrix.data = x$get()
  invs = solve(matrix.data, ...)
  #  set the value of the inverse matrix and return the inverse matrix
  x$setinvs(invs)
  return(invs)
}
